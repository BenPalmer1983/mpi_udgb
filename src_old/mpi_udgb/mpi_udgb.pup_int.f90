SUBROUTINE pup_int_0(var_in, loop, method)
!###########################################################################
INTEGER(kind=StandardInteger), INTENT(INOUT) :: var_in
!INTEGER(kind=StandardInteger), INTENT(IN) :: loop
!INTEGER(kind=StandardInteger), INTENT(IN) :: method
INTEGER(kind=StandardInteger) :: a, b
!###########################################################################
!# BROADCAST
IF(method .EQ. 1)THEN
  IF (loop .eq. 1 .AND. proc_id .NE. 0) THEN


  ELSE IF (loop .eq. 2 .AND. proc_id .NE. 0) THEN


  ELSE IF (loop .eq. 3 .AND. proc_id .EQ. 0) THEN  


  END IF
  
!# GATHER  
ELSE IF(method .EQ. 2)THEN
  IF (loop .eq. 1 .AND. proc_id .NE. 0) THEN


  ELSE IF (loop .eq. 2 .AND. proc_id .NE. 0) THEN


  ELSE IF (loop .eq. 3 .AND. proc_id .EQ. 0) THEN  


  END IF
END IF
END SUBROUTINE pup_int_0



SUBROUTINE pup_int_1(var_in, loop, method)
!###########################################################################
INTEGER(kind=StandardInteger), INTENT(INOUT), DIMENSION(:) :: var_in
INTEGER(kind=StandardInteger), INTENT(IN) :: loop
INTEGER(kind=StandardInteger), INTENT(IN) :: method
INTEGER(kind=StandardInteger) :: a, b, rows
!###########################################################################
!# BROADCAST
IF(method .EQ. 1)THEN
  IF (loop .eq. 1 .AND. proc_id .NE. 0) THEN


  ELSE IF (loop .eq. 2 .AND. proc_id .NE. 0) THEN


  ELSE IF (loop .eq. 3 .AND. proc_id .EQ. 0) THEN  


  END IF
  
!# GATHER  
ELSE IF(method .EQ. 2)THEN
  IF (loop .eq. 1 .AND. proc_id .NE. 0) THEN


  ELSE IF (loop .eq. 2 .AND. proc_id .NE. 0) THEN


  ELSE IF (loop .eq. 3 .AND. proc_id .EQ. 0) THEN  


  END IF
END IF
END SUBROUTINE pup_int_1



SUBROUTINE pup_int_2(var_in, loop, method)
!###########################################################################
INTEGER(kind=StandardInteger), INTENT(INOUT), DIMENSION(:,:) :: var_in
INTEGER(kind=StandardInteger), INTENT(IN) :: loop
INTEGER(kind=StandardInteger), INTENT(IN) :: method
INTEGER(kind=StandardInteger) :: a, b, rows
!###########################################################################
!# BROADCAST
IF(method .EQ. 1)THEN
  IF (loop .eq. 1 .AND. proc_id .NE. 0) THEN


  ELSE IF (loop .eq. 2 .AND. proc_id .NE. 0) THEN


  ELSE IF (loop .eq. 3 .AND. proc_id .EQ. 0) THEN  


  END IF
  
!# GATHER  
ELSE IF(method .EQ. 2)THEN
  IF (loop .eq. 1 .AND. proc_id .NE. 0) THEN


  ELSE IF (loop .eq. 2 .AND. proc_id .NE. 0) THEN


  ELSE IF (loop .eq. 3 .AND. proc_id .EQ. 0) THEN  


  END IF
END IF
END SUBROUTINE pup_int_2



SUBROUTINE pup_int_3(var_in, loop, method)
!###########################################################################
INTEGER(kind=StandardInteger), INTENT(INOUT), DIMENSION(:,:,:) :: var_in
INTEGER(kind=StandardInteger), INTENT(IN) :: loop
INTEGER(kind=StandardInteger), INTENT(IN) :: method
INTEGER(kind=StandardInteger) :: a, b, rows
!###########################################################################
!# BROADCAST
IF(method .EQ. 1)THEN
  IF (loop .eq. 1 .AND. proc_id .NE. 0) THEN


  ELSE IF (loop .eq. 2 .AND. proc_id .NE. 0) THEN


  ELSE IF (loop .eq. 3 .AND. proc_id .EQ. 0) THEN  


  END IF
  
!# GATHER  
ELSE IF(method .EQ. 2)THEN
  IF (loop .eq. 1 .AND. proc_id .NE. 0) THEN


  ELSE IF (loop .eq. 2 .AND. proc_id .NE. 0) THEN


  ELSE IF (loop .eq. 3 .AND. proc_id .EQ. 0) THEN  


  END IF
END IF
END SUBROUTINE pup_int_3