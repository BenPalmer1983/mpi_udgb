SUBROUTINE pup_dp_0(var_in)
!###########################################################################
REAL(kind=DoubleReal), INTENT(INOUT) :: var_in
!###########################################################################
!# BROADCAST
IF(method .EQ. 1)THEN
  IF (loop .eq. 1 .AND. proc_id .EQ. 0) THEN
    counter(3) = counter(3) + 1
  ELSE IF (loop .eq. 2 .AND. proc_id .EQ. 0) THEN
    bdp(npos(3)) = var_in
    npos(3) = npos(3) + 1
  ELSE IF (loop .eq. 3 .AND. proc_id .NE. 0) THEN  
    var_in = bdp(npos(3))
    npos(3) = npos(3) + 1    ! INT COUNT
  END IF
!###############  
!# GATHER  
ELSE IF(method .EQ. 2)THEN
  IF (loop .eq. 1 .AND. proc_id .NE. 0 .AND. (mod(n-1, proc_count) .EQ. proc_id)) THEN
    counter(3) = counter(3) + 1    ! INT COUNT
  ELSE IF (loop .EQ. 2 .AND. proc_id .NE. 0 .AND. (mod(n-1, proc_count) .EQ. proc_id)) THEN
    bdp(npos(3)) = var_in
    npos(3) = npos(3) + 1
  ELSE IF (loop .GE. 3 .AND. proc_id .EQ. 0 .AND. (mod(n-1, proc_count) .EQ. worker_id)) THEN
    var_in = bdp(npos(3))
    npos(3) = npos(3) + 1    ! INT COUNT
  END IF
END IF
END SUBROUTINE pup_dp_0



SUBROUTINE pup_dp_1(var_in)
!###########################################################################
REAL(kind=DoubleReal), ALLOCATABLE, INTENT(INOUT) :: var_in(:)
!###########################################################################
!# BROADCAST
IF(method .EQ. 1)THEN
  IF (loop .eq. 1 .AND. proc_id .EQ. 0) THEN
    counter(1) = counter(1) + 1                 ! HEADER COUNT
    IF(ALLOCATED(var_in))THEN
      counter(3) = counter(3) + SIZE(var_in, 1)   ! DP COUNT
    END IF
  ELSE IF (loop .eq. 2 .AND. proc_id .EQ. 0) THEN
    IF(ALLOCATED(var_in))THEN  
      ! SAVE SIZE
      bheader(npos(1)) = SIZE(var_in, 1)
      npos(1) = npos(1) + 1
      ! SAVE VALUES
      bdp(npos(3):npos(3) + SIZE(var_in, 1) - 1) = var_in(:)
      npos(3) = npos(3)  + SIZE(var_in, 1)
    ELSE
      ! SAVE SIZE
      bheader(npos(1)) = 0
      npos(1) = npos(1) + 1
    END IF       
  ELSE IF (loop .eq. 3 .AND. proc_id .GT. 0) THEN  
    IF(bheader(npos(1)) .EQ. 0)THEN
      npos(1) = npos(1) + 1                     ! INCREMENT BHEADER COUNTER  
    ELSE
      ! ALLOCATE
      IF((ALLOCATED(var_in) .AND. SIZE(var_in,1) .NE. bheader(npos(1))))THEN
        DEALLOCATE(var_in)
      END IF  
      IF(.NOT. ALLOCATED(var_in)) THEN
        ALLOCATE(var_in(1:bheader(npos(1))))
      END IF 
      ! SET DATA CHANGE COUNTERS
      var_in(:) = bdp(npos(3):npos(3) + bheader(npos(1)) - 1)    
      npos(3) = npos(3) + bheader(npos(1))      ! INCREMENT BINT COUNTER
      npos(1) = npos(1) + 1                     ! INCREMENT BHEADER COUNTER  
    END IF 
  END IF
!###############
!# GATHER  
ELSE IF(method .EQ. 2)THEN
  IF (loop .EQ. 1 .AND. proc_id .NE. 0 .AND. (mod(n-1, proc_count) .EQ. proc_id)) THEN
    counter(1) = counter(1) + 1                 ! HEADER COUNT
    IF(ALLOCATED(var_in))THEN
      counter(3) = counter(3) + SIZE(var_in, 1)   ! DP COUNT
    END IF
  ELSE IF (loop .EQ. 2 .AND. proc_id .NE. 0 .AND. (mod(n-1, proc_count) .EQ. proc_id)) THEN
    IF(ALLOCATED(var_in))THEN
      ! SAVE SIZE
      bheader(npos(1)) = SIZE(var_in, 1)
      npos(1) = npos(1) + 1
      ! SAVE VALUES
      bdp(npos(3):npos(3) + SIZE(var_in, 1) - 1) = var_in(:)
      npos(3) = npos(3)  + SIZE(var_in, 1)
    ELSE
      ! SAVE SIZE
      bheader(npos(1)) = 0
      npos(1) = npos(1) + 1
    END IF   
  ELSE IF (loop .GE. 3 .AND. proc_id .EQ. 0  .AND.  (mod(n-1, proc_count) .EQ. worker_id)) THEN
    IF(bheader(npos(1)) .EQ. 0)THEN
      npos(1) = npos(1) + 1                     ! INCREMENT BHEADER COUNTER  
    ELSE
      ! ALLOCATE
      IF((ALLOCATED(var_in) .AND. SIZE(var_in,1) .NE. bheader(npos(1))))THEN
        DEALLOCATE(var_in)
      END IF  
      IF(.NOT. ALLOCATED(var_in)) THEN
        ALLOCATE(var_in(1:bheader(npos(1))))
      END IF 
      ! SET DATA CHANGE COUNTERS
      var_in(:) = bdp(npos(3):npos(3) + bheader(npos(1)) - 1)    
      npos(3) = npos(3) + bheader(npos(1))      ! INCREMENT BDP COUNTER
      npos(1) = npos(1) + 1                     ! INCREMENT BHEADER COUNTER   
    END IF
  END IF
END IF
END SUBROUTINE pup_dp_1



SUBROUTINE pup_dp_2(var_in)
!###########################################################################
REAL(kind=DoubleReal), ALLOCATABLE, INTENT(INOUT) :: var_in(:,:)
INTEGER(kind=StandardInteger) :: row
!###########################################################################
!# BROADCAST
IF(method .EQ. 1)THEN
  IF (loop .eq. 1 .AND. proc_id .EQ. 0) THEN  
    counter(1) = counter(1) + 1                 ! HEADER COUNT
    IF(ALLOCATED(var_in))THEN
      counter(1) = counter(1) + 1
      counter(3) = counter(3) + SIZE(var_in, 1) * SIZE(var_in, 2)
    END IF
  ELSE IF (loop .eq. 2 .AND. proc_id .EQ. 0) THEN
    IF(ALLOCATED(var_in))THEN
      ! SAVE SIZES
      bheader(npos(1)) = SIZE(var_in, 1)
      bheader(npos(1) + 1) = SIZE(var_in, 2)
      npos(1) = npos(1) + 2
      ! SAVE VALUES
      DO row=1,SIZE(var_in, 1)
        bdp(npos(3):npos(3) + SIZE(var_in, 2) - 1) = var_in(row, :)
        npos(3) = npos(3) + SIZE(var_in, 2)
      END DO
    ELSE
      ! SAVE SIZE
      bheader(npos(1)) = 0
      npos(1) = npos(1) + 1
    END IF
  ELSE IF (loop .eq. 3 .AND. proc_id .NE. 0) THEN 
    IF(bheader(npos(1)) .EQ. 0)THEN
      npos(1) = npos(1) + 1                     ! INCREMENT BHEADER COUNTER  
    ELSE  
      ! ALLOCATE
      IF(ALLOCATED(var_in))THEN
        DEALLOCATE(var_in)
      END IF  
      ALLOCATE(var_in(1:bheader(npos(1)),1:bheader(npos(1)+1)))
      ! SET DATA CHANGE COUNTERS
      DO row=1, bheader(npos(1))
        var_in(row, :) = bdp(npos(3):npos(3) + bheader(npos(1)+1) - 1)  
        npos(3) = npos(3) + bheader(npos(1)+1)
      END DO
      npos(1) = npos(1) + 2  
    END IF
  END IF
!###############  
!# GATHER  
ELSE IF(method .EQ. 2)THEN
  IF (loop .EQ. 1 .AND. proc_id .NE. 0 .AND. (mod(n-1, proc_count) .EQ. proc_id)) THEN
    counter(1) = counter(1) + 1                 ! HEADER COUNT
    IF(ALLOCATED(var_in))THEN
      counter(1) = counter(1) + 1
      counter(3) = counter(3) + SIZE(var_in, 1) * SIZE(var_in, 2)
    END IF
  ELSE IF (loop .EQ. 2 .AND. proc_id .NE. 0 .AND. (mod(n-1, proc_count) .EQ. proc_id)) THEN
    IF(ALLOCATED(var_in))THEN
      ! SAVE SIZES
      bheader(npos(1)) = SIZE(var_in, 1)
      bheader(npos(1) + 1) = SIZE(var_in, 2)
      npos(1) = npos(1) + 2
      ! SAVE VALUES
      DO row=1,SIZE(var_in, 1)
        bdp(npos(3):npos(3) + SIZE(var_in, 2) - 1) = var_in(row, :)
        npos(3) = npos(3) + SIZE(var_in, 2)
      END DO
    ELSE
      ! SAVE SIZE
      bheader(npos(1)) = 0
      npos(1) = npos(1) + 1
    END IF
  ELSE IF (loop .GE. 3 .AND. proc_id .EQ. 0  .AND.  (mod(n-1, proc_count) .EQ. worker_id)) THEN 
    IF(bheader(npos(1)) .EQ. 0)THEN
      npos(1) = npos(1) + 1                     ! INCREMENT BHEADER COUNTER  
    ELSE
      ! ALLOCATE
      IF(ALLOCATED(var_in))THEN
        DEALLOCATE(var_in)
      END IF  
      ALLOCATE(var_in(1:bheader(npos(1)),1:bheader(npos(1)+1)))
      ! SET DATA CHANGE COUNTERS
      DO row=1, bheader(npos(1))
        var_in(row, :) = bdp(npos(3):npos(3) + bheader(npos(1)+1) - 1)  
        npos(3) = npos(3) + bheader(npos(1)+1)
      END DO
      npos(1) = npos(1) + 2  
    END IF
  END IF
END IF
END SUBROUTINE pup_dp_2



SUBROUTINE pup_dp_3(var_in)
!###########################################################################
REAL(kind=DoubleReal), INTENT(INOUT) :: var_in(:,:,:)
INTEGER(kind=StandardInteger) :: a, b, rows
!###########################################################################
!# BROADCAST
IF(method .EQ. 1)THEN
  IF (loop .eq. 1 .AND. proc_id .EQ. 0) THEN
    counter(1) = counter(1) + 3
    counter(3) = counter(3) + SIZE(var_in, 1) * SIZE(var_in, 2) * SIZE(var_in, 3)


  ELSE IF (loop .eq. 2 .AND. proc_id .EQ. 0) THEN


  ELSE IF (loop .eq. 3 .AND. proc_id .NE. 0) THEN  


  END IF
  
!# GATHER  
ELSE IF(method .EQ. 2)THEN
  IF (loop .eq. 1 .AND. proc_id .NE. 0) THEN


  ELSE IF (loop .eq. 2 .AND. proc_id .NE. 0) THEN


  ELSE IF (loop .eq. 3 .AND. proc_id .EQ. 0) THEN  


  END IF
END IF
END SUBROUTINE pup_dp_3