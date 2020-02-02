  

  ! END TYPE ARRAY LOOP 
  END DO

  ! BROADCAST
  IF(method .EQ. 1)THEN
    IF(loop .EQ. 1)THEN 
      CALL MPI_BCAST(counter, 10, MPI_INTEGER, 0, MPI_COMM_WORLD, error) 
      ALLOCATE(bheader(1:counter(1)))
      ALLOCATE(bint(1:counter(2)))
      ALLOCATE(bdp(1:counter(3)))
      ALLOCATE(blogical(1:counter(4)))    
    END IF
  
    ! BROADCAST BUFFERS OUT FROM ROOT TO WORKERS
    IF(method .EQ. 1 .AND. loop .EQ. 2)THEN 
      CALL MPI_BCAST(bheader, SIZE(bheader,1), MPI_INTEGER, 0, MPI_COMM_WORLD, error)   
      CALL MPI_BCAST(bint, SIZE(bint,1), MPI_INTEGER, 0, MPI_COMM_WORLD, error)   
      CALL MPI_BCAST(bdp, SIZE(bdp,1), MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, error)   
      CALL MPI_BCAST(blogical, SIZE(blogical,1), MPI_LOGICAL, 0, MPI_COMM_WORLD, error)   
    END IF
    
    ! FREE MEMORY
    IF(method .EQ. 1 .AND. loop .EQ. 3)THEN 
      DEALLOCATE(bheader)
      DEALLOCATE(bint)
      DEALLOCATE(bdp)
      DEALLOCATE(blogical)    
    END IF
  END IF
  
  
  ! GATHER
  IF(method .EQ. 2)THEN
    ! GATHER:  ALLOCATE BUFFER MEMORY ON WORKERS
    IF(loop .EQ. 1 .AND. proc_id .NE. 0)THEN 
      ALLOCATE(bheader(1:counter(1)))
      ALLOCATE(bint(1:counter(2)))
      ALLOCATE(bdp(1:counter(3)))
      ALLOCATE(blogical(1:counter(4)))    
    END IF
    
    ! GATHER:  DEALLOCATE MEMORY ON BUFFER
    IF((loop .GE. 3 .AND. proc_id .EQ. 0) .OR. (loop .EQ. loop_max .AND. proc_id .NE. 0))THEN
      DEALLOCATE(bheader)
      DEALLOCATE(bint)
      DEALLOCATE(bdp)
      DEALLOCATE(blogical)   
    END IF    
  END IF
! END LOOP
END DO
