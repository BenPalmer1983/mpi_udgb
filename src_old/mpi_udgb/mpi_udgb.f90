!######################################################################
!#                               GATHER
!#          
!#          Assumes each element of type array has been calculated
!#          or completed proc_id = mod(n-1, proc_count) n=1,total
!#          
!#          
!######################################################################


!######################################################################
!#                               BROADCAST
!#          
!#          Broadcasts all the elements in the array from the root
!#          out to the workers.  The array must be there and allocated,
!#          but arrays that make up the type are allocated automatically.
!#          
!######################################################################


MODULE mpi_udgb

USE mpi
USE kinds
USE data_types

IMPLICIT NONE

! MPI VARIABLES
INTEGER(kind=StandardInteger) ::                               proc_id
INTEGER(kind=StandardInteger) ::                               proc_count
INTEGER(kind=StandardInteger) ::                               error
INTEGER(kind=StandardInteger), DIMENSION(MPI_STATUS_SIZE) ::   status

! BUFFERS
INTEGER(kind=StandardInteger), ALLOCATABLE, DIMENSION(:) ::    bheader        ! HEADER DATA
INTEGER(kind=StandardInteger), ALLOCATABLE, DIMENSION(:) ::    bint           ! INTEGERS
REAL(kind=DoubleReal), ALLOCATABLE, DIMENSION(:) ::            bdp            ! DOUBLES
LOGICAL, ALLOCATABLE, DIMENSION(:) ::                          blogical       ! LOGICALS

! COUNTERS
INTEGER(kind=StandardInteger) ::                               loop           ! LOOP 1,2,3


INTEGER(kind=StandardInteger) ::                               ecount         ! ARRAY ELEMENT COUNT
INTEGER(kind=StandardInteger) ::                               ecounter       ! ARRAY ELEMENT COUNT
INTEGER(kind=StandardInteger) ::                               kcount
INTEGER(kind=StandardInteger) ::                               intcount
INTEGER(kind=StandardInteger) ::                               dpcount
INTEGER(kind=StandardInteger) ::                               lcount
INTEGER(kind=StandardInteger) ::                               intoffset
INTEGER(kind=StandardInteger) ::                               kpos
INTEGER(kind=StandardInteger) ::                               ipos
INTEGER(kind=StandardInteger) ::                               dppos
INTEGER(kind=StandardInteger) ::                               lpos
  
! PACK/UNPACK
INTERFACE pup
MODULE PROCEDURE pup_int_0, pup_int_1, pup_int_2, pup_int_3, &
                 pup_dp_0, pup_dp_1, pup_dp_2, pup_dp_3 
END INTERFACE pup

!INTERFACE pup_b
!MODULE PROCEDURE pup_b_int_1, pup_b_int_2, pup_b_int_3, &
!                 pup_b_dp_1, pup_b_dp_2, pup_b_dp_3, &
!                 pup_b_l_1, pup_b_l_2, pup_b_l_3
!END INTERFACE pup_b


CONTAINS

INCLUDE "mpi_udgb.pup_int.f90"
INCLUDE "mpi_udgb.pup_dp.f90"
INCLUDE "types/mpi_udgb.t_data1.f90"

!INCLUDE "mpi_udgb.t_config.f90"
!INCLUDE "mpi_udgb.t_cghosts.f90"
!INCLUDE "mpi_udgb.t_efs.f90"
!INCLUDE "mpi_udgb.t_neighbourlist.f90"
!INCLUDE "mpi_udgb.t_potential.f90"
!INCLUDE "mpi_udgb.pup.f90"


END MODULE mpi_udgb