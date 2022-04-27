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
* \file output_driver.cpp
* \ingroup Objects
* \brief OutputDriver source file.
* \author Jim Naslund
*/

#include "util/base/include/definitions.h"

#include <cassert>

#include "emissions/include/output_driver.h"
#include "technologies/include/ioutput.h"
#include "util/base/include/xml_helper.h"

using namespace std;

double OutputDriver::calcEmissionsDriver( const vector<IInput*>& aInputs,
                                         const vector<IOutput*>& aOutputs,
                                         const int aPeriod ) const
{
    return aOutputs[ 0 ]->getPhysicalOutput( aPeriod );
}

OutputDriver* OutputDriver::clone() const {
    return new OutputDriver( *this );
}

const string& OutputDriver::getXMLName() const {
    return getXMLNameStatic();
}

const string& OutputDriver::getXMLNameStatic(){
    static const string XML_NAME = "output-driver";
    return XML_NAME;
}

void OutputDriver::toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLName(), aOut, aTabs );
    XMLWriteClosingTag( getXMLName(), aOut, aTabs );
}
