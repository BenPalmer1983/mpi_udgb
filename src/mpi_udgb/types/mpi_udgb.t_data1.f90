! EDIT SUBROUTINE NAME AND USER DEFINED TYPE
!############################################################################################

SUBROUTINE run_mpi_test(v, method_in)
!###########################################################################
TYPE(t_data1), INTENT(INOUT), ALLOCATABLE :: v(:)
INTEGER(kind=StandardInteger), INTENT(IN) :: method_in     ! 1 BROADCAST     2 GATHER
!###########################
INCLUDE "mpi_udgb._section_a.f90"
!############################################################################################
CALL pup(v(n)%switch)
CALL pup(v(n)%n)
CALL pup(v(n)%energy_a)
CALL pup(v(n)%energy_b)
CALL pup(v(n)%stress)
CALL pup(v(n)%center_a)
CALL pup(v(n)%n_arr_2d)
CALL pup(v(n)%n_arr_1d)
CALL pup(v(n)%center_b)
CALL pup(v(n)%center_c)
CALL pup(v(n)%center_d)
CALL pup(v(n)%n_arr)
!############################################################################################
INCLUDE "mpi_udgb._section_b.f90"
END SUBROUTINE run_mpi_test


! OPTIONAL 
! COMMENT OUT IF NOT REQUIRED
!############################################################################################

SUBROUTINE mpi_broadcast_test(v)
TYPE(t_data1), INTENT(INOUT), ALLOCATABLE :: v(:)
CALL run_mpi_test(v, 1)
END SUBROUTINE mpi_broadcast_test


SUBROUTINE mpi_gather_test(v)
TYPE(t_data1), INTENT(INOUT), ALLOCATABLE :: v(:)
CALL run_mpi_test(v, 2)
END SUBROUTINE mpi_gather_test