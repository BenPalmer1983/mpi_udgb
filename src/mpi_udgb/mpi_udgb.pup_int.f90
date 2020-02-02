SUBROUTINE pup_int_0(var_in)
!###########################################################################
INTEGER(kind=StandardInteger), INTENT(INOUT) :: var_in
INTEGER(kind=StandardInteger) :: a, b
!###########################################################################
!# BROADCAST
IF(method .EQ. 1)THEN
  IF (loop .EQ. 1 .AND. proc_id .EQ. 0) THEN
    counter(2) = counter(2) + 1    ! INT COUNT
  ELSE IF (loop .EQ. 2 .AND. proc_id .EQ. 0) THEN
    bint(npos(2)) = var_in
    npos(2) = npos(2) + 1
  ELSE IF (loop .EQ. 3 .AND. proc_id .NE. 0) THEN  
    var_in = bint(npos(2))
    npos(2) = npos(2) + 1    ! INT COUNT
  END IF
!###############  
!# GATHER  
ELSE IF(method .EQ. 2)THEN
  IF (loop .eq. 1 .AND. proc_id .NE. 0 .AND. (mod(n-1, proc_count) .EQ. proc_id)) THEN
    counter(2) = counter(2) + 1    ! INT COUNT
  ELSE IF (loop .EQ. 2 .AND. proc_id .NE. 0 .AND. (mod(n-1, proc_count) .EQ. proc_id)) THEN
    bint(npos(2)) = var_in
    npos(2) = npos(2) + 1
  ELSE IF (loop .GE. 3 .AND. proc_id .EQ. 0 .AND. (mod(n-1, proc_count) .EQ. worker_id)) THEN
    var_in = bint(npos(2))
    npos(2) = npos(2) + 1    ! INT COUNT
  END IF
END IF
END SUBROUTINE pup_int_0



SUBROUTINE pup_int_1(var_in)
!###########################################################################
INTEGER(kind=StandardInteger), ALLOCATABLE, INTENT(INOUT) :: var_in(:)
!###########################################################################
!# BROADCAST
IF(method .EQ. 1)THEN
  IF (loop .EQ. 1 .AND. proc_id .EQ. 0) THEN
    counter(1) = counter(1) + 1                 ! HEADER COUNT
    counter(2) = counter(2) + SIZE(var_in, 1)   ! INT COUNT
  ELSE IF (loop .EQ. 2 .AND. proc_id .EQ. 0) THEN
    ! SAVE SIZE
    bheader(npos(1)) = SIZE(var_in, 1)
    npos(1) = npos(1) + 1
    ! SAVE VALUES
    bint(npos(2):npos(2) + SIZE(var_in, 1) - 1) = var_in(:)
    npos(2) = npos(2)  + SIZE(var_in, 1)
  ELSE IF (loop .EQ. 3 .AND. proc_id .GT. 0) THEN  
    ! ALLOCATE
    IF((ALLOCATED(var_in) .AND. SIZE(var_in,1) .NE. bheader(npos(1))))THEN
      DEALLOCATE(var_in)
    END IF  
    IF(.NOT. ALLOCATED(var_in)) THEN
      ALLOCATE(var_in(1:bheader(npos(1))))
    END IF 
    ! SET DATA CHANGE COUNTERS
    var_in(:) = bint(npos(2):npos(2) + bheader(npos(1)) - 1)    
    npos(2) = npos(2) + bheader(npos(1))      ! INCREMENT BINT COUNTER
    npos(1) = npos(1) + 1                     ! INCREMENT BHEADER COUNTER  
  END IF  
!###############
!# GATHER  
ELSE IF(method .EQ. 2)THEN
  IF (loop .EQ. 1 .AND. proc_id .NE. 0 .AND. (mod(n-1, proc_count) .EQ. proc_id)) THEN
    counter(1) = counter(1) + 1                 ! HEADER COUNT
    counter(2) = counter(2) + SIZE(var_in, 1)   ! INT COUNT
  ELSE IF (loop .EQ. 2 .AND. proc_id .NE. 0 .AND. (mod(n-1, proc_count) .EQ. proc_id)) THEN
    ! SAVE SIZE
    bheader(npos(1)) = SIZE(var_in, 1)
    npos(1) = npos(1) + 1
    ! SAVE VALUES
    bint(npos(2):npos(2) + SIZE(var_in, 1) - 1) = var_in(:)
    npos(2) = npos(2)  + SIZE(var_in, 1)
  ELSE IF (loop .GE. 3 .AND. proc_id .EQ. 0  .AND.  (mod(n-1, proc_count) .EQ. worker_id)) THEN
    ! ALLOCATE
    IF((ALLOCATED(var_in) .AND. SIZE(var_in,1) .NE. bheader(npos(1))))THEN
      DEALLOCATE(var_in)
    END IF  
    IF(.NOT. ALLOCATED(var_in)) THEN
      ALLOCATE(var_in(1:bheader(npos(1))))
    END IF 
    ! SET DATA CHANGE COUNTERS
    var_in(:) = bint(npos(2):npos(2) + bheader(npos(1)) - 1)    
    npos(2) = npos(2) + bheader(npos(1))      ! INCREMENT BINT COUNTER
    npos(1) = npos(1) + 1                     ! INCREMENT BHEADER COUNTER  
  END IF
END IF
END SUBROUTINE pup_int_1




SUBROUTINE pup_int_2(var_in)
!###########################################################################
INTEGER(kind=StandardInteger), ALLOCATABLE, INTENT(INOUT) :: var_in(:,:)
INTEGER(kind=StandardInteger) :: row
!###########################################################################
!# BROADCAST
IF(method .EQ. 1)THEN
  IF (loop .EQ. 1 .AND. proc_id .EQ. 0) THEN
    counter(1) = counter(1) + 2
    counter(2) = counter(2) + SIZE(var_in, 1) * SIZE(var_in, 2)
  ELSE IF (loop .EQ. 2 .AND. proc_id .EQ. 0) THEN
    ! SAVE SIZES
    bheader(npos(1)) = SIZE(var_in, 1)
    bheader(npos(1) + 1) = SIZE(var_in, 2)
    npos(1) = npos(1) + 2
    ! SAVE VALUES
    DO row=1,SIZE(var_in, 1)
      bint(npos(2):npos(2) + SIZE(var_in, 1) - 1) = var_in(row, :)
      npos(2) = npos(2) + SIZE(var_in, 2)
    END DO
  ELSE IF (loop .EQ. 3 .AND. proc_id .NE. 0) THEN  
    ! ALLOCATE
    IF(ALLOCATED(var_in))THEN
      DEALLOCATE(var_in)
    END IF  
    ALLOCATE(var_in(1:bheader(npos(1)),1:bheader(npos(1)+1)))
    ! SET DATA CHANGE COUNTERS
    DO row=1, bheader(npos(1))
      var_in(row, :) = bint(npos(2):npos(2) + bheader(npos(1)+1) - 1)  
      npos(2) = npos(2) + bheader(npos(1)+1)
    END DO
    npos(1) = npos(1) + 2  
  END IF
!###############  
!# GATHER  
ELSE IF(method .EQ. 2)THEN
  IF (loop .EQ. 1 .AND. proc_id .NE. 0 .AND. (mod(n-1, proc_count) .EQ. proc_id)) THEN
    counter(1) = counter(1) + 2
    counter(2) = counter(2) + SIZE(var_in, 1) * SIZE(var_in, 2)
  ELSE IF (loop .EQ. 2 .AND. proc_id .NE. 0 .AND. (mod(n-1, proc_count) .EQ. proc_id)) THEN
    ! SAVE SIZES
    bheader(npos(1)) = SIZE(var_in, 1)
    bheader(npos(1) + 1) = SIZE(var_in, 2)
    npos(1) = npos(1) + 2
    ! SAVE VALUES
    DO row=1,SIZE(var_in, 1)
      bint(npos(2):npos(2) + SIZE(var_in, 1) - 1) = var_in(row, :)
      npos(2) = npos(2) + SIZE(var_in, 2)
    END DO
  ELSE IF (loop .GE. 3 .AND. proc_id .EQ. 0  .AND.  (mod(n-1, proc_count) .EQ. worker_id)) THEN 
    ! ALLOCATE
    IF(ALLOCATED(var_in))THEN
      DEALLOCATE(var_in)
    END IF  
    ALLOCATE(var_in(1:bheader(npos(1)),1:bheader(npos(1)+1)))
    ! SET DATA CHANGE COUNTERS
    DO row=1, bheader(npos(1))
      var_in(row, :) = bint(npos(2):npos(2) + bheader(npos(1)+1) - 1)  
      npos(2) = npos(2) + bheader(npos(1)+1)
    END DO
    npos(1) = npos(1) + 2  
  END IF
END IF
END SUBROUTINE pup_int_2



SUBROUTINE pup_int_3(var_in)
!###########################################################################
INTEGER(kind=StandardInteger), ALLOCATABLE, INTENT(INOUT) :: var_in(:,:,:)
INTEGER(kind=StandardInteger) :: a, b, rows
!###########################################################################
!# BROADCAST
IF(method .EQ. 1)THEN
  IF (loop .EQ. 1 .AND. proc_id .EQ. 0) THEN
    counter(1) = counter(1) + 3
    counter(2) = counter(2) + SIZE(var_in, 1) * SIZE(var_in, 2) * SIZE(var_in, 3)


  ELSE IF (loop .EQ. 2 .AND. proc_id .EQ. 0) THEN

  ELSE IF (loop .EQ. 3 .AND. proc_id .NE. 0) THEN  


  END IF
  
!# GATHER  
ELSE IF(method .EQ. 2)THEN
  IF (loop .EQ. 1 .AND. proc_id .NE. 0) THEN


  ELSE IF (loop .EQ. 2 .AND. proc_id .NE. 0) THEN


  ELSE IF (loop .EQ. 3 .AND. proc_id .EQ. 0) THEN  


  END IF
END IF
END SUBROUTINE pup_int_3