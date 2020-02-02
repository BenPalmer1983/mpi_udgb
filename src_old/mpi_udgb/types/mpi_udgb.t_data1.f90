
SUBROUTINE run_mpi_test(type_in, method)
!###########################################################################
TYPE(t_data1), INTENT(INOUT) :: type_in(:)
INTEGER(kind=StandardInteger), INTENT(IN) :: method
INCLUDE "mpi_udgb._top.f90"
!############################################################################################






!############################################################################################
INCLUDE "mpi_udgb._bottom.f90"
END SUBROUTINE run_mpi_test