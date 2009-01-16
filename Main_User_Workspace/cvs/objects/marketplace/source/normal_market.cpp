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
* \file normal_market.cpp
* \ingroup Objects
* \brief NormalMarket class source file.
* \author Sonny Kim
*/

#include <string>
#include "util/base/include/definitions.h"
#include "util/base/include/util.h"
#include "marketplace/include/normal_market.h"

using namespace std;

//! Constructor
NormalMarket::NormalMarket( const string& goodNameIn, const string& regionNameIn, const int periodIn ) :
Market( goodNameIn, regionNameIn, periodIn ) {
}

void NormalMarket::toDebugXMLDerived( ostream& out, Tabs* tabs ) const {
}

IMarketType::Type NormalMarket::getType() const {
    return IMarketType::NORMAL;
}

void NormalMarket::initPrice() {
   Market::initPrice();
}

void NormalMarket::setPrice( const double priceIn ) {
    Market::setPrice( priceIn );
}

void NormalMarket::set_price_to_last_if_default( const double lastPrice ) {
   Market::set_price_to_last_if_default( lastPrice );
}

void NormalMarket::set_price_to_last( const double lastPrice ) {
   Market::set_price_to_last( lastPrice );
}

double NormalMarket::getPrice() const {
    return Market::getPrice();
}

void NormalMarket::addToDemand( const double demandIn ) {
    Market::addToDemand( demandIn );
}

double NormalMarket::getDemand() const {
    return Market::getDemand();
}

void NormalMarket::nullSupply() {
   Market::nullSupply();
}

double NormalMarket::getSupply() const {
    return Market::getSupply();
}

void NormalMarket::addToSupply( const double supplyIn ) {
    Market::addToSupply( supplyIn );
}

bool NormalMarket::meetsSpecialSolutionCriteria() const {
    return Market::meetsSpecialSolutionCriteria();
}

bool NormalMarket::shouldSolve() const {
    bool doSolveMarket = false;
    // Check if this market is a type that is solved.
    if ( solveMarket ) {
        // If demand exists, then solve.
        if( demand > 0 ) {
            doSolveMarket = true;
        }
    }
    return doSolveMarket;
}

bool NormalMarket::shouldSolveNR() const {
   return ( solveMarket && price > 0 && demand > 0 && supply > 0 );
}
