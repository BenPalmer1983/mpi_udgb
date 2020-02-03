! EDIT SUBROUTINE NAME AND USER DEFINED TYPE
!############################################################################################

SUBROUTINE run_mpi_potential(v, method_in)
!###########################################################################
TYPE(t_potential), INTENT(INOUT), ALLOCATABLE :: v(:)
INTEGER(kind=StandardInteger), INTENT(IN) :: method_in     ! 1 BROADCAST     2 GATHER
!###########################
INCLUDE "mpi_udgb._section_a.f90"
!############################################################################################
CALL pup(v(n)%f_on)
CALL pup(v(n)%f_type)
CALL pup(v(n)%a)
CALL pup(v(n)%b)
CALL pup(v(n)%group)
CALL pup(v(n)%rcut)
CALL pup(v(n)%zoor)
CALL pup(v(n)%a_on)
CALL pup(v(n)%a_type)
CALL pup(v(n)%a_params)
CALL pup(v(n)%a_lower)
CALL pup(v(n)%a_upper)
CALL pup(v(n)%a_point_count)
CALL pup(v(n)%p_count_raw)
CALL pup(v(n)%points_raw)
CALL pup(v(n)%points)
CALL pup(v(n)%px_min)
CALL pup(v(n)%px_max)
CALL pup(v(n)%px_d)
CALL pup(v(n)%px_range)
!############################################################################################
INCLUDE "mpi_udgb._section_b.f90"
END SUBROUTINE run_mpi_potential


! OPTIONAL 
! COMMENT OUT IF NOT REQUIRED
!############################################################################################

SUBROUTINE run_broadcast_potential(v)
TYPE(t_potential), INTENT(INOUT), ALLOCATABLE :: v(:)
CALL run_mpi_potential(v, 1)
END SUBROUTINE run_broadcast_potential


SUBROUTINE run_gather_potential(v)
TYPE(t_potential), INTENT(INOUT), ALLOCATABLE :: v(:)
CALL run_mpi_potential(v, 2)
END SUBROUTINE run_gather_potential