/*
 * LEGAL NOTICE
 * This computer software was prepared by Battelle Memorial Institute,
 * hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
 * with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
 * CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
 * LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
 * sentence must appear on any copies of this computer software.
 * 
 * EXPORT CONTROL
 * User agrees that the Software will not be shipped, transferred or
 * exported into any country or used in any manner prohibited by the
 * United States Export Administration Act or any other applicable
 * export laws, restrictions or regulations (collectively the "Export Laws").
 * Export of the Software may require some form of license or other
 * authority from the U.S. Government, and failure to obtain such
 * export control license may result in criminal liability under
 * U.S. laws. In addition, if the Software is identified as export controlled
 * items under the Export Laws, User represents and warrants that User
 * is not a citizen, or otherwise located within, an embargoed nation
 * (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
 *     and that User is not otherwise prohibited
 * under the Export Laws from receiving the Software.
 * 
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */

/*! 
* \file bisect_one.cpp
* \ingroup objects
* \brief BisectOne class source file.
* \author Josh Lurz
*/

#include "util/base/include/definitions.h"
#include <string>

#include "solution/solvers/include/solver_component.h"
#include "solution/solvers/include/bisect_one.h"
#include "solution/util/include/calc_counter.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/world.h"
#include "solution/util/include/solver_info.h"
#include "solution/util/include/solver_info_set.h"
#include "solution/util/include/solver_library.h"
#include "util/base/include/util.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/configuration.h"

using namespace std;

const string BisectOne::SOLVER_NAME = "BisectOne";

//! Default Constructor. Constructs the base class. 
BisectOne::BisectOne( Marketplace* marketplaceIn, World* worldIn, CalcCounter* calcCounterIn ):SolverComponent( marketplaceIn, worldIn, calcCounterIn ) {
}

//! Init method. Currently does nothing.
void BisectOne::init() {
}

//! Get the name of the SolverComponent
const string& BisectOne::getName() const {
    return SOLVER_NAME;
}

//! Get the name of the SolverComponent
const string& BisectOne::getNameStatic() {
    return SOLVER_NAME;
}

/*! \brief Bisection the worst market.
* \details Bisect the the worst market.
* \param solutionTolerance Target value for maximum relative solution for worst
*        market 
* \param edSolutionFloor *Absolute value* beneath which market is ignored
* \param maxIterations Maximum number of iterations the subroutine will
*        periodform. 
* \param solverSet Object which contains a set of objects with information on
*        each market.
* \param period Model periodiod
*/
SolverComponent::ReturnCode BisectOne::solve( const double solutionTolerance, const double edSolutionFloor,
                                              const unsigned int maxIterations, SolverInfoSet& solverSet,
                                              const int period )
{
    startMethod();

    // Constants.
     // Maximum number of iterations without improvement.
    const static unsigned int MAX_ITER_NO_IMPROVEMENT = 8;
    // Read in the bracket interval until MiniCAM and SGM can consistently solve
    // with the same one.
    const Configuration* conf = Configuration::getInstance();
    static const double BRACKET_INTERVAL = conf->getDouble( "bracket-interval", 0.5 );
    // Setup logging.
    ILogger& solverLog = ILogger::getLogger( "solver_log" );
    solverLog.setLevel( ILogger::NOTICE );

    ILogger& worstMarketLog = ILogger::getLogger( "worst_market_log" );
    ILogger& singleLog = ILogger::getLogger( "single_market_log" );
    singleLog.setLevel( ILogger::DEBUG );

    // Make sure we have all updated information.
    solverSet.updateFromMarkets();
    solverSet.updateSolvable( false );

    // Select the worst market.
    // SolverInfo& worstSol = solverSet.getWorstSolverInfoReverse( solutionTolerance, 0, true );
    SolverInfo* worstSol = solverSet.getWorstSolverInfo( edSolutionFloor );
    // worstSol.setBisectedFlag();
    solverLog.setLevel( ILogger::NOTICE );
    solverLog << "BisectOne function called on market " << worstSol->getName() << "." << endl;
    SolverLibrary::bracketOne( marketplace, world, BRACKET_INTERVAL, solutionTolerance,
                               edSolutionFloor, solverSet, worstSol, calcCounter, period );
    unsigned int numIterations = 0;
    do {
        solverSet.printMarketInfo( "Bisect One on " + worstSol->getName(), calcCounter->getPeriodCount(), singleLog );

        // Move the right price bracket in if Supply > Demand
        if ( worstSol->getED() < 0 ) {
            worstSol->moveRightBracketToX();
        }
        // Move the left price bracket in if Demand >= Supply
        else {
            worstSol->moveLeftBracketToX();
        }
        // Set new trial value to center
        worstSol->setPriceToCenter();

        solverSet.updateToMarkets();
        marketplace->nullSuppliesAndDemands( period );

        world->calc( period );
        solverSet.updateFromMarkets();
        solverSet.updateSolvable( false );
        addIteration( worstSol->getName(), worstSol->getRelativeED( edSolutionFloor ) );
        worstMarketLog << "BisectOne-MaxRelED: "  << *worstSol << endl;
        solverLog << "BisectOneWorst-MaxRelED: " << *worstSol << endl;
    } // end do loop        
    while ( ( ++numIterations < maxIterations ) &&
              !worstSol->isSolved( solutionTolerance, edSolutionFloor ) );
    // Report results.
    solverLog.setLevel( ILogger::NOTICE );
    if( numIterations >= maxIterations ){
        solverLog << "Exiting BisectOne due to reaching max iterations." << endl;
    }
    else if( !isImproving( MAX_ITER_NO_IMPROVEMENT ) ){
        solverLog << "Exiting BisectOne due to lack of improvement." << endl;
    }
    else {
        solverLog << "Exiting BisectOne because chosen market is solved." << endl;
    }
    return solverSet.isAllSolved( solutionTolerance, edSolutionFloor ) ? SUCCESS: FAILURE_ITER_MAX_REACHED; // WRONG ERROR CODE
}
