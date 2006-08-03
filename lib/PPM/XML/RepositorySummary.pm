#
# PPM::XML::PPMConfig
#
# Definition of the PPMConfig file format; configuration options for the Perl
# Package Manager.
#
###############################################################################

$PPM::XML::RepositorySummary::revision = '$Id: RepositorySummary.pm,v 1.1 2005/05/22 05:06:21 rkobes Exp $';
$PPM::XML::RepositorySummary::VERSION  = '0.01';

###############################################################################
# Import everything from PPM::XML::PPD into our own namespace.
###############################################################################
package PPM::XML::RepositorySummary;
use PPM::XML::PPD ':elements';

###############################################################################
# RepositorySummary Element: Characters
###############################################################################
package PPM::XML::RepositorySummary::Characters;
@ISA = qw( PPM::XML::Element );

###############################################################################
# RepositorySummary Element: REPOSITORYSUMMARY
###############################################################################
package PPM::XML::RepositorySummary::REPOSITORYSUMMARY;
@ISA = qw( PPM::XML::ValidatingElement );
@okids  = qw( SOFTPKG );

__END__
