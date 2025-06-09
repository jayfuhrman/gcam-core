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
 * Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
 * Distributed as open-source under the terms of the Educational Community
 * License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
 *
 * For further details, see: http://www.globalchange.umd.edu/models/gcam/
 *
 */


/*!
 * \file pass_through_sector.cpp
 * \ingroup Objects
 * \brief PassThroughSector class source file.
 * \author Pralit Patel
 */

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>

// xml headers
#include "sectors/include/pass_through_sector.h"
#include "sectors/include/subsector.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/market_dependency_finder.h"
#include "containers/include/iinfo.h"
#include "util/logger/include/ilogger.h"

#include "sectors/include/sector_utils.h"
#include "functions/include/idiscrete_choice.hpp"

using namespace std;

extern Scenario* scenario;

/* \brief Constructor
 */
PassThroughSector::PassThroughSector():
SupplySector()
{
}

const string& PassThroughSector::getXMLNameStatic() {
    const static string XML_NAME = "pass-through-sector";
    return XML_NAME;
}

const string& PassThroughSector::getXMLName() const {
    return getXMLNameStatic();
}

void PassThroughSector::toDebugXMLDerived( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteElement( mMarginalRevenueSector, "marginal-revenue-sector", aOut, aTabs );
    XMLWriteElementCheckDefault( mMarginalRevenueMarket, "marginal-revenue-market", aOut, aTabs, mRegionName );
}

void PassThroughSector::completeInit( const IInfo* aRegionInfo,
                                      ILandAllocator* aLandAllocator )
{
    SupplySector::completeInit( aRegionInfo, aLandAllocator );

    if( mMarginalRevenueSector.empty() ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "No marginal revenue sector set for pass-through sector " << mName << " in " << mRegionName << "." << endl;
        // TODO: allow this or just quit?
        mMarginalRevenueSector = mName;
    }

    // The default market for the marginal revenue sector is just the same region
    // as this sector.
    if( mMarginalRevenueMarket.empty() ) {
        mMarginalRevenueMarket = mRegionName;
    }

    // Add dependencies for a calc item to gather up the fixed demands from this
    // pass through sector and make that available for the downstream sector
    const string fixedDemandActivityName = mName + "-fixed-output";
    MarketDependencyFinder* depFinder = scenario->getMarketplace()->getDependencyFinder();

    // Ensure we gather the fixed demands after we calculate prices / before we
    // set supplies
    depFinder->addDependency( fixedDemandActivityName, mRegionName, mName, mRegionName );
    depFinder->addDependency( fixedDemandActivityName, mRegionName, mMarginalRevenueSector, mMarginalRevenueMarket );

    // Set the activity to create the call back.
    // Note the market dependency finder will manage the newly allocated memory.
    depFinder->resolveActivityToDependency( mRegionName, fixedDemandActivityName,
                                            new CalcFixedOutputActivity( this ) );
    
    // Create an unsolved trial value market for passing the fixed output which is
    // the safest way to do so to ensure proper behavior when calculating partial
    // derivatives.
    Marketplace* marketplace = scenario->getMarketplace();
    marketplace->createMarket( mRegionName,
                               mRegionName,
                               fixedDemandActivityName,
                               IMarketType::TRIAL_VALUE );
    
    // Set price and output units for period 0 market info
    IInfo* marketInfoTrialSupplySector = marketplace->getMarketInfo( fixedDemandActivityName, mRegionName, 0, true );
    marketInfoTrialSupplySector->setString( "price-unit", mOutputUnit );
    marketInfoTrialSupplySector->setString( "output-unit", mOutputUnit );
    
    // This trial market is used purely to pass data between sectors and should
    // never be solved.
    for( int per = 0; per < scenario->getModeltime()->getmaxper(); ++per ){
        marketplace->unsetMarketToSolve( fixedDemandActivityName, mRegionName, per );
    }
}

void PassThroughSector::initCalc( const Demographic* aDemographics,
                                  const int aPeriod )
{
    SupplySector::initCalc( aDemographics, aPeriod );
}

double PassThroughSector::getFixedOutput( const int aPeriod ) const {
    const double marginalRevenue = scenario->getMarketplace()->getPrice( mMarginalRevenueSector,
        mMarginalRevenueMarket, aPeriod );
    double totalfixedOutput = 0;
    for( CSubsectorIterator subSecIter = mSubsectors.begin(); subSecIter != mSubsectors.end(); subSecIter++ ) {
        totalfixedOutput += (*subSecIter)->getFixedOutput( aPeriod, marginalRevenue );
    }
    return totalfixedOutput;
}

/*!
 * \brief Calculate the total fixed output in this sector (including vintaged output)
 *        and add it to the market where it can be retrieved downstream.
 * \param aPeriod The current model period.
 */
void PassThroughSector::setFixedDemandsToMarket( const int aPeriod ) const {
    const_cast<PassThroughSector*>( this )->mLastCalcFixedOutput = getFixedOutput( aPeriod );

    const string fixedDemandActivityName = mName + "-fixed-output";
    Marketplace* marketplace = scenario->getMarketplace();
    // set the fixed out to both sides of the equation (supply=price for trial markets)
    // so the solver doesn't complain it is "unsolved"
    marketplace->addToDemand( fixedDemandActivityName, mRegionName,
                              const_cast<PassThroughSector*>( this )->mLastCalcFixedOutput, aPeriod );
    marketplace->setPrice( fixedDemandActivityName, mRegionName, mLastCalcFixedOutput, aPeriod );
}

CalcFixedOutputActivity::CalcFixedOutputActivity( const PassThroughSector* aSector ):
mSector( aSector )
{
}

CalcFixedOutputActivity::~CalcFixedOutputActivity() {
    // Note that this object does not own any memory
}

void CalcFixedOutputActivity::calc( const int aPeriod ) {
    mSector->setFixedDemandsToMarket( aPeriod );
}

string CalcFixedOutputActivity::getDescription() const {
    return mSector->mRegionName + " " + mSector->getName() + "-fixed-output";
}

/* \brief Constructor
 */
ProfitRateSector::ProfitRateSector():
SupplySector()
{
}

const string& ProfitRateSector::getXMLNameStatic() {
    const static string XML_NAME = "profit-rate-sector";
    return XML_NAME;
}

const string& ProfitRateSector::getXMLName() const {
    return getXMLNameStatic();
}

void ProfitRateSector::completeInit( const IInfo* aRegionInfo,
                                     ILandAllocator* aLandAllocator )
{
    SupplySector::completeInit( aRegionInfo, aLandAllocator );

    if( mZeroProfitMarketName.empty() ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "No zero profit constraint market name in " << mName << " in " << mRegionName << "." << endl;
        abort();
    }

    // TODO: we should add dependencies just to be sure even though the techs will
    // likely add the right ones already
    //MarketDependencyFinder* depFinder = scenario->getMarketplace()->getDependencyFinder();

    // Ensure we gather the fixed demands after we calculate prices / before we
    // set supplies
    //depFinder->addDependency( fixedDemandActivityName, mRegionName, mName, mRegionName );
    //depFinder->addDependency( fixedDemandActivityName, mRegionName, mMarginalRevenueSector, mMarginalRevenueMarket );
}

const vector<double> ProfitRateSector::calcChildShares( const int aPeriod, std::pair<double, double>& oUnnormalizedShareSum ) const {
    // Calculate unnormalized shares.
    vector<double> subsecShares( mSubsectors.size() );
    for( unsigned int i = 0; i < mSubsectors.size(); ++i ){
        subsecShares[ i ] = mSubsectors[ i ]->calcShare( mDiscreteChoiceModel, aPeriod );
    }

    // Normalize the shares.  After normalization they will be true shares, not log(shares).
    oUnnormalizedShareSum = SectorUtils::normalizeLogShares( subsecShares );
    if( oUnnormalizedShareSum.first == 0.0 ) {
        // This should no longer happen, but it's still technically possible.
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::DEBUG );
        mainLog << "Shares for sector " << mName << " in region " << mRegionName
                << " did not normalize correctly. Sum is " << oUnnormalizedShareSum.first << " * exp( "
                << oUnnormalizedShareSum.second << " ) "<< "." << endl;
    }

    return subsecShares;
}

double ProfitRateSector::getPrice( const int aPeriod ) const {
    /*pair<double, double> unnormalizedSum;
    const vector<double>& subsecShares = calcChildShares( aPeriod, unnormalizedSum );
    

    if( unnormalizedSum.first < util::getSmallNumber() ) {
        // TODO: I don't think this is still true
        
        // None of the subsector have a valid share.  Set the price
        // to NaN.  This gets tested in calcShare(), and any subsector
        // with a NaN price gets a share of zero.  Therefore, as long
        // as you use only subsectors with positive shares, you will
        // never see the NaN price.
        return numeric_limits<double>::signaling_NaN();
    }
    else {
        double profitRate = mDiscreteChoiceModel->calcAverageValue( unnormalizedSum.first, unnormalizedSum.second, aPeriod );
        return profitRate;
    }*/
    return SupplySector::getPrice(aPeriod);
}

void ProfitRateSector::calcFinalSupplyPrice( const int aPeriod ) {
    // Instruct all subsectors to calculate their costs. This must be done
    // before prices can be calculated.
    calcCosts( aPeriod );

    // Set the price into the market.
    Marketplace* marketplace = scenario->getMarketplace();

    mProfitRate = getPrice( aPeriod );
    
    // set the calculated profit rate into the zero profit constraint market
    marketplace->addToDemand(mZeroProfitMarketName, mRegionName, mProfitRate, aPeriod);

    // set the price of the sector as well even though no one will be using it
    // TODO: just set zero?
    marketplace->setPrice( mName, mRegionName, mProfitRate, aPeriod, true );
}