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
 * \file mac_control.cpp
 * \ingroup Objects
 * \brief MACControl class source file.
 * \author Kate Calvin
 */

#include "util/base/include/definitions.h"

#include <cmath>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "emissions/include/mac_control.h"
#include "containers/include/scenario.h"
#include "util/base/include/xml_helper.h"
#include "util/logger/include/ilogger.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/iinfo.h"
#include "containers/include/market_dependency_finder.h"
#include "util/curves/include/point_set_curve.h"
#include "util/curves/include/explicit_point_set.h"
#include "util/curves/include/xy_data_point.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

//! Default constructor.
MACControl::MACControl():
AEmissionsControl(),
mNoZeroCostReductions( false ),
mTechChange( new objects::PeriodVector<double>( 0.0 ) ),
mPhaseInFraction( new objects::PeriodVector<double>( 1.0 ) ),
mZeroCostPhaseInTime( 25 ),
mCovertPriceValue( 1 ),
mPriceMarketName( "CO2" ),
mMacCurve( new PointSetCurve( new ExplicitPointSet() ) )
{
}

//! Default destructor.
MACControl::~MACControl(){
    delete mMacCurve;
}

//! Copy constructor.
MACControl::MACControl( const MACControl& aOther )
: AEmissionsControl( aOther ) {
    mMacCurve = 0;
    copy( aOther );
}

//! Clone operator.
MACControl* MACControl::clone() const {
    return new MACControl( *this );
}

//! Assignment operator.
MACControl& MACControl::operator=( const MACControl& aOther ){
    if( this != &aOther ){
        // Free memory before copying.  Since this is just a single
        // variable I am just deleting it directly here.
        delete mMacCurve;
        mMacCurve = 0;
        AEmissionsControl::operator=( aOther );
        copy( aOther );
    }
    return *this;
}

//! Copy helper function.
void MACControl::copy( const MACControl& aOther ){
    /*!
     * \pre mMacCurve should be null otherwise we have a memory leak.
     */
    assert( !mMacCurve );
    mMacCurve = aOther.mMacCurve->clone();
    mNoZeroCostReductions = aOther.mNoZeroCostReductions;
    mTechChange = aOther.mTechChange;
    mPhaseInFraction = aOther.mPhaseInFraction;
    mZeroCostPhaseInTime = aOther.mZeroCostPhaseInTime;
    mCovertPriceValue = aOther.mCovertPriceValue;
    mPriceMarketName = aOther.mPriceMarketName;
}

/*!
 * \brief Get the XML node name for output to XML.
 * \details This public function accesses the private constant string, XML_NAME.
 *          This way the tag is always consistent for both read-in and output and can be easily changed.
 *          This function may be virtual to be overridden by derived class pointers.
 * \author Jim Naslund
 * \return The constant XML_NAME.
 */
const string& MACControl::getXMLName() const {
    return getXMLNameStatic();
}

const string& MACControl::getXMLNameStatic(){
    static const string XML_NAME = "mac-control";
    return XML_NAME;
}

bool MACControl::XMLDerivedClassParse( const string& aNodeName, const DOMNode* aCurrNode ){
    const Modeltime* modeltime = scenario->getModeltime();
    if ( aNodeName == "mac-reduction" ){
        double taxVal = XMLHelper<double>::getAttr( aCurrNode, "tax" );
        double reductionVal = XMLHelper<double>::getValue( aCurrNode );
        XYDataPoint* currPoint = new XYDataPoint( taxVal, reductionVal );
        mMacCurve->getPointSet()->addPoint( currPoint );
    }
    else if ( aNodeName == "no-zero-cost-reductions" ){
        mNoZeroCostReductions = true;
    }
    else if ( aNodeName == "tech-change" ){
        XMLHelper<double>::insertValueIntoVector( aCurrNode, *mTechChange, modeltime );
    }
    else if ( aNodeName == "phase-in-fraction" ){
        XMLHelper<double>::insertValueIntoVector( aCurrNode, *mPhaseInFraction, modeltime );
    }
    else if ( aNodeName == "zero-cost-phase-in-time" ){
        mZeroCostPhaseInTime = XMLHelper<int>::getValue( aCurrNode );
    }
    else if ( aNodeName == "mac-price-conversion" ){
        mCovertPriceValue = XMLHelper<Value>::getValue( aCurrNode );
    }
    else if ( aNodeName == "market-name" ){
        mPriceMarketName = XMLHelper<string>::getValue( aCurrNode );
    }
    else{
        return false;
    }    
    return true;
}

void MACControl::toDebugXMLDerived( const int period, ostream& aOut, Tabs* aTabs ) const {
    const vector<pair<double,double> > pairs = mMacCurve->getSortedPairs();
    typedef vector<pair<double, double> >::const_iterator PairIterator;
    map<string, double> attrs;
    for( PairIterator currPair = pairs.begin(); currPair != pairs.end(); ++currPair ) {
        attrs[ "tax" ] = currPair->first;
        XMLWriteElementWithAttributes( currPair->second, "mac-reduction", aOut, aTabs, attrs );
    }
    const Modeltime* modeltime = scenario->getModeltime();

    XMLWriteElementCheckDefault( mZeroCostPhaseInTime, "zero-cost-phase-in-time", aOut, aTabs, 25 );
    XMLWriteElementCheckDefault( mNoZeroCostReductions, "no-zero-cost-reductions", aOut, aTabs, false );
    XMLWriteElementCheckDefault( mCovertPriceValue, "mac-price-conversion", aOut, aTabs, Value( 1.0 ) );
    XMLWriteElement( mPriceMarketName, "market-name", aOut, aTabs );
    XMLWriteElement( mNoZeroCostReductions, "no-zero-cost-reductions", aOut, aTabs);
	XMLWriteElement( (*mTechChange)[ period ], "tech-change", aOut, aTabs );
    XMLWriteElement( (*mPhaseInFraction)[ period ], "phase-in-fraction", aOut, aTabs );
}

void MACControl::completeInit( const string& aRegionName, const string& aSectorName,
                               const IInfo* aTechInfo )
{
    scenario->getMarketplace()->getDependencyFinder()->addDependency( aSectorName, aRegionName, mPriceMarketName, aRegionName );

    if ( mMacCurve->getMaxX() == -DBL_MAX ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "MAC Curve " << getName() << " appears to have no data. " << endl;
    }
}

void MACControl::initCalc( const string& aRegionName,
                           const IInfo* aTechInfo,
                           const NonCO2Emissions* aParentGHG,
                           const int aPeriod )
{
}

void MACControl::calcEmissionsReduction( const std::string& aRegionName, const int aPeriod, const GDP* aGDP ) {
    // Check first if MAC curve operation should be turned off
    if ( mCovertPriceValue < 0 ) { // User flag to turn off MAC curves
        setEmissionsReduction( 0 );
        return;
    }
    
    const Marketplace* marketplace = scenario->getMarketplace();
    double emissionsPrice = marketplace->getPrice( mPriceMarketName, aRegionName, aPeriod, false );
    if( emissionsPrice == Marketplace::NO_MARKET_PRICE ) {
        emissionsPrice = 0;
    }
    
    emissionsPrice *= mCovertPriceValue;

    double reduction = getMACValue( emissionsPrice );
    reduction = adjustForTechChange( aPeriod, reduction );
    
    if( mNoZeroCostReductions && emissionsPrice <= 0.0 ) {
        reduction = 0.0;
    }

    // Define a carbon price value to reflect what is considered to be a "high carbon price"
    // at which point any reductions that are being phased in (see below) are assumed to
    // be 100% implemented due to economic incentives provided by a high price.
    // Set this price in units of $1990/tC
    // Price is deliberately set to be high as substantial incentive is presumed to be
    // needed to overcome historical near-term inertia, retrofit old vintages, etc.
    // This will only work, in these units, for GHGs, not air pollutants, but the two
    // phase-in options below are not envisioned to be appropriate for air pollutants.
    const double macCarbonPricePhaseInLimit = 400;
    
    // Adjust to smoothly phase-in "no-cost" emission reductions
    // Some MAC curves have non-zero abatement at zero emissions price. Unless the users sets
    // mNoZeroCostReductions, this reduction will occur even without an emissions price. This
    // code smoothly phases in this abatement so that a sudden change in emissions does not
    // occur. The phase-in period has a default value that can be altered
    // by the user. This code also reduces this phase-in period if there is a emissions-price,
    // which avoids an illogical situation where a high emissions price is present and mitigation
    // is maxed out, but the "no-cost" reductions are not fully phased in.
    const int lastCalYear = scenario->getModeltime()->getper_to_yr( 
                            scenario->getModeltime()->getFinalCalibrationPeriod() );
    int modelYear = scenario->getModeltime()->getper_to_yr( aPeriod );

    // Amount of zero-cost reduction
    double zeroCostReduction = getMACValue( 0 );

    if ( ( reduction > 0.0 ) && ( zeroCostReduction > 0.0 ) &&
            ( modelYear <= ( lastCalYear + mZeroCostPhaseInTime ) ) )
        {
            // Fraction of zero cost that is removed from original reduction value
            // Equal to 1 at last calibration year and zero at the zero cost phase in time
            double multiplier = ( static_cast<double>( lastCalYear ) + mZeroCostPhaseInTime
                                - static_cast<double>( modelYear ) ) / mZeroCostPhaseInTime;
    
            // If emissions price is not zero, accelerate the phase in for consistency if there are
            // zero cost reductions to phase in
            double adjEmissionsPrice = min( emissionsPrice, macCarbonPricePhaseInLimit );
            multiplier *= ( macCarbonPricePhaseInLimit - adjEmissionsPrice ) / macCarbonPricePhaseInLimit;
            
            reduction = reduction - zeroCostReduction * multiplier;
        }
    
    // Determine any phase-in of the reduction
    // phaseInFraction offers users an option to make additional adjustment for the existing MAC
    // reductions due to factors other than "no-cost" emission reductions and technological changes
    // A primary purpose is to smoothly phase in MACs in early modeling periods. In some cases, the
    // first MAC year (as well as carbon price year) would lead to a dramatic yet unrealistic
    // MAC-driven emission reductions, and phaseInFraction(period) can be applied to tune those first
    // several modeling periods;
    // A second purpose is to allow users to explore scenarios when different regions have different
    // MAC phase-in periods.
    // The phaseInFraction is initially set as 1 by default, and should be between 0 and 1.
    // If a user assigns a phaseInFraction smaller than 0 or greater than 1, it will be ignored.
    double phaseInFraction = 1;
    if ( (*mPhaseInFraction)[ aPeriod ] >= 0 && (*mPhaseInFraction)[ aPeriod ] <= 1 ) {
      phaseInFraction = (*mPhaseInFraction)[ aPeriod ];

        // Adjust to provide a carbon-price response
        // here we set a carbon price ceiling as the maxEmissionsTax to reflect a point
        // when we think carbon price is "high enough" to fully dimish any smooth phase-in process
        if( emissionsPrice > 0 ) {
            // here define an adjusted emission price as the difference between current emission price and
            // maxEmissionsTax; if emissionsPrice > maxEmissionsTax, then this adjusted emission price is 0
            double adjEmissionsPricePhaseIn = min( emissionsPrice, macCarbonPricePhaseInLimit );
            
            // then multiplierPhaseIn is from 0 to 1 as price increases, when emissionsPrice is greater than
            // maxEmissionsTax, it will be kept as 1
            // here this function is nonlinear to the power of 1.5, and the slope is also increasing as emissionsPrice goes up
            // For reference, when maxEmissionsTax is set as 400($1990/tC), the multiplier will be 0.13 when emission price
            // is 100; 0.35 when emission price is 200, and 0.65 when emimission price is 300, 0.82 when emission price is
            // 350, and 0.96 when emission price is 390.
            double multiplierPhaseIn = pow( adjEmissionsPricePhaseIn / macCarbonPricePhaseInLimit, 1.5);
            
            // Finally just the phaseInFraction based on the calculated multiplierPhaseIn
            // here, when multiplierPhaseIn = 0, phaseInFraction will be itself
            // when multiplierPhaseIn is closer to 1, phaseInFraction will be diminished to 1
            phaseInFraction = phaseInFraction + ( 1 - phaseInFraction) * multiplierPhaseIn;
        }

        reduction = reduction * phaseInFraction;
    }
    
    setEmissionsReduction( reduction );
}

/*! \brief Get MAC curve value
 *  Wrapper function that takes care of error handling for MAC curve values.
 *  If there is an error, a value of zero is returned and a message is logged.
 * \param aCarbonPrice carbon price
 */
double MACControl::getMACValue( const double aCarbonPrice ) const {
    const double maxCO2Tax = mMacCurve->getMaxX();
    
    // so that getY function won't interpolate beyond last value
    double effectiveCarbonPrice = min( aCarbonPrice, maxCO2Tax );

    double reduction = mMacCurve->getY( effectiveCarbonPrice );

    // If no mac curve read in then reduction should be zero.
    // This is a legitimate option for a user to remove a mac curve
    if ( ( mMacCurve->getMinX() == mMacCurve->getMaxX() ) && ( mMacCurve->getMaxX() == 0 ) ) {
         reduction = 0;
    }
    // Check to see if some other error has occurred
    else if ( reduction == -DBL_MAX ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << " An error occured when evaluating MAC curve for a GHG." << endl;
        reduction = 0;
    }
    
    return reduction;
}

/*! \brief Adjust for Tech Change
 *  Function that applies tech change to MAC curves, shifting them upwards
 * \param aPeriod period for reduction
 * \param reduction pre-tech change reduction
 */
double MACControl::adjustForTechChange( const int aPeriod, double reduction ) {

    // note technical change is a rate of change per year, therefore we must
    // be sure to apply it for as many years as are in a model time step
    double techChange = 1;
    int timestep;
    for ( int i=1; i <= aPeriod; i++ ) {
        timestep = scenario->getModeltime()->gettimestep( i );
        techChange *= pow( 1 + (*mTechChange)[ i ], timestep );
    }
    reduction *= techChange;
    
    // TODO: Include read-in max reduction -- some sectors really shouldn't be able to reduce 100%. We could allow a read-in maximum
    if ( reduction > 1 ) {
        reduction = 1;
    }
    
    return reduction;
}

