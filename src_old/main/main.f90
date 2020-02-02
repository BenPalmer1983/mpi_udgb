PROGRAM mpi_udgb_test
! University of Birmingham
! Ben Palmer

USE mpi
USE kinds
USE data_types
USE arr_fill
USE mpi_udgb, ONLY: run_mpi_test


IMPLICIT NONE

CALL main()

CONTAINS

SUBROUTINE main()
!#################################
INTEGER(KIND=StandardInteger) :: n, b
INTEGER(KIND=StandardInteger) :: error
INTEGER(kind=StandardInteger) :: proc_id, proc_count
INTEGER(kind=StandardInteger), DIMENSION(MPI_STATUS_SIZE) :: status
!#################################
TYPE(t_data1) :: tsingle
TYPE(t_data1), ALLOCATABLE :: tarray(:)




CALL MPI_Init(error)
b = 0

! Get MPI details
Call MPI_Comm_rank(MPI_COMM_WORLD,proc_id,error)
Call MPI_Comm_size(MPI_COMM_WORLD,proc_count,error)

print *, "Test ", proc_id, "/", proc_count

CALL MPI_BCAST(b, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, error) 

ALLOCATE(tarray(1:1))

IF(proc_id .EQ. 0)THEN

tarray(1)%n = 5
tarray(1)%energy_a = 6.0D0
tarray(1)%energy_b = 7.0D0

ALLOCATE(tarray(1)%stress(1:9, 1:9))
CALL fill_2d_dp(tarray(1)%stress)

ALLOCATE(tarray(1)%center_a(1:9))
CALL fill_1d_dp(tarray(1)%center_a)

ALLOCATE(tarray(1)%n_arr_2d(1:4, 1:4))
CALL fill_2d_int(tarray(1)%n_arr_2d)

ALLOCATE(tarray(1)%n_arr_1d(1:10))
CALL fill_1d_int(tarray(1)%n_arr_1d)

ALLOCATE(tarray(1)%center_b(1:10))
CALL fill_1d_dp(tarray(1)%center_b)

tarray(1)%center_c(:) = 2.1D0
tarray(1)%center_d(:,:) = 1.1D0
tarray(1)%n_arr(:) = 29

END IF



print *, proc_id, tarray(1)%energy_b 
CALL MPI_BCAST(b, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, error) 

!run_mpi_test
CALL run_mpi_test(tarray, 1)


CALL MPI_Finalize(error)
CALL EXIT(0)

END SUBROUTINE main




END PROGRAM mpi_udgb_test

