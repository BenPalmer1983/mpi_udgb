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
INTEGER(KIND=StandardInteger) :: n, arr_size
INTEGER(KIND=StandardInteger) :: error
INTEGER(kind=StandardInteger) :: proc_id, proc_count
INTEGER(kind=StandardInteger), DIMENSION(MPI_STATUS_SIZE) :: status
!#################################
TYPE(t_data1) :: tsingle
TYPE(t_data1), ALLOCATABLE :: tarray(:)




CALL MPI_Init(error)

! Get MPI details
Call MPI_Comm_rank(MPI_COMM_WORLD,proc_id,error)
Call MPI_Comm_size(MPI_COMM_WORLD,proc_count,error)


!#############################
!  BROADCAST EXAMPLE
!#############################
arr_size = 2000
ALLOCATE(tarray(1:arr_size))
DO n=1,arr_size
IF(proc_id .EQ. 0)THEN
tarray(n)%n = 5 * n
tarray(n)%energy_a = n * 6.0D0
tarray(n)%energy_b = n * 7.0D0
ALLOCATE(tarray(n)%stress(1:9, 1:9))
CALL fill_2d_dp(tarray(n)%stress)
ALLOCATE(tarray(n)%center_a(1:9))
CALL fill_1d_dp(tarray(n)%center_a)
ALLOCATE(tarray(n)%n_arr_2d(1:3, 1:3))
CALL fill_2d_int(tarray(n)%n_arr_2d)
ALLOCATE(tarray(n)%n_arr_1d(1:3))
CALL fill_1d_int(tarray(n)%n_arr_1d)
ALLOCATE(tarray(n)%center_b(1:10))
CALL fill_1d_dp(tarray(n)%center_b)
ALLOCATE(tarray(n)%center_c(1:5))
tarray(n)%center_c(:) = 2.1D0
ALLOCATE(tarray(n)%center_d(1:5, 1:5))
tarray(n)%center_d(:,:) = 1.1D0
ALLOCATE(tarray(n)%n_arr(1:2*n))
tarray(n)%n_arr(:) = 29
END IF
END DO

print *, "BEFORE ", proc_id, tarray(6)%n, SIZE(tarray(6)%n_arr)
CALL run_mpi_test(tarray, 1)
CALL SLEEP(1)
print *, "AFTER ", proc_id, tarray(6)%n, SIZE(tarray(6)%n_arr)
CALL SLEEP(1)



DEALLOCATE(tarray)

arr_size = 20
ALLOCATE(tarray(1:arr_size))
DO n=1,arr_size

IF(mod(n-1,proc_count) .EQ. proc_id)THEN

tarray(n)%n = 5 * n
tarray(n)%energy_a = n * 6.0D0
tarray(n)%energy_b = n * 7.0D0
ALLOCATE(tarray(n)%stress(1:9, 1:9))
CALL fill_2d_dp(tarray(n)%stress)
ALLOCATE(tarray(n)%center_a(1:9))
CALL fill_1d_dp(tarray(n)%center_a)
ALLOCATE(tarray(n)%n_arr_2d(1:3, 1:3))
CALL fill_2d_int(tarray(n)%n_arr_2d)
ALLOCATE(tarray(n)%n_arr_1d(1:3))
CALL fill_1d_int(tarray(n)%n_arr_1d)
ALLOCATE(tarray(n)%center_b(1:10))
CALL fill_1d_dp(tarray(n)%center_b)
ALLOCATE(tarray(n)%center_c(1:5))
tarray(n)%center_c(:) = 2.1D0
ALLOCATE(tarray(n)%center_d(1:5, 1:5))
tarray(n)%center_d(:,:) = 1.1D0
ALLOCATE(tarray(n)%n_arr(1:2*n))
tarray(n)%n_arr(:) = 29
END IF
END DO

print *, "BEFORE ", proc_id, tarray(6)%n, SIZE(tarray(6)%n_arr)
CALL run_mpi_test(tarray, 2)
CALL SLEEP(1)
print *, "MIDWAY ", proc_id, tarray(6)%n, SIZE(tarray(6)%n_arr)
CALL run_mpi_test(tarray, 1)
CALL SLEEP(1)
print *, "AFTER ", proc_id, tarray(6)%n, SIZE(tarray(6)%n_arr)





!print *, proc_id, tarray(1)%energy_b  

!run_mpi_test
!CALL run_mpi_test(tarray, 1)

!print *, proc_id, tarray(1)%n
!print *, proc_id, tarray(1)%energy_a
!print *, proc_id, tarray(1)%energy_b
!print *, proc_id, tarray(1)%stress
!print *, proc_id, tarray(1)%center_a
!print *, proc_id, tarray(1)%n_arr_2d(:,:)
!print *, proc_id, tarray(1)%n_arr_1d(:)
!print *, proc_id, tarray(1)%center_b(:)
!print *, proc_id, tarray(1)%center_c(:)
!print *, proc_id, tarray(1)%center_d(:,:)
!print *, proc_id, tarray(1)%n_arr(:)
!print *, ""


CALL MPI_Finalize(error)
CALL EXIT(0)

END SUBROUTINE main




END PROGRAM mpi_udgb_test

