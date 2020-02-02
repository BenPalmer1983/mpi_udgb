
!###############################
! Store Method

method = method_in


!###############################
! Set proc_id, proc_count
CALL MPI_Comm_rank(MPI_COMM_WORLD,proc_id,error)
CALL MPI_Comm_size(MPI_COMM_WORLD,proc_count,error)

!
!# Reset Counter
counter = 0

! BROADCAST - SEND TYPE ARRAY SIZE TO ALL
IF (proc_id .EQ. 0) THEN
  tarr_size = SIZE(v,1)
END IF
CALL MPI_BCAST(tarr_size, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, error) 
IF (method .EQ. 1) THEN
  IF (proc_id .GT. 0) THEN
    IF(ALLOCATED(v))THEN
      IF(SIZE(v,1) .NE. tarr_size)THEN
        DEALLOCATE(v)
        ALLOCATE(v(1:tarr_size))
      END IF
    ELSE
      ALLOCATE(v(1:tarr_size))
    END IF
  END IF
END IF

! BROADCAST
! Loop 1 calc size of buffers
! Loop 2 pack and broadcast on root    
! Loop 3 receive and unpack by workers
loop_max = 3

! GATHER    n element array, each process holds where mod((n-1), proc_count) = proc_id
! Loop 1 calc size of buffers
! Loop 2 pack and broadcast on workers
IF(method .EQ. 2)THEN
  loop_max = proc_count + 1 
END IF

DO loop = 1,loop_max
  ! BROADCAST AND GATHER - RESET BUFFER COUNTERS
  IF(loop .GE. 2)THEN
    npos = 1
  END IF

  ! GATHER ONLY
  IF(method .EQ. 2 .AND. loop .GE. 3)THEN
    worker_id = loop - 2  
    from = loop - 2  
    tag(1) = worker_id + 10000
    tag(2) = worker_id + 20000
    tag(3) = worker_id + 30000
    tag(4) = worker_id + 40000
    tag(5) = worker_id + 50000
    
    !# SEND BUFFER SIZES
    IF(proc_id .EQ. worker_id)THEN   
      CALL MPI_SEND(counter, SIZE(counter), MPI_INTEGER, 0, tag(1), MPI_COMM_WORLD, error)
    ELSE IF(proc_id .EQ. 0)THEN
      CALL MPI_RECV(counter, SIZE(counter), MPI_INTEGER, worker_id, tag(1), MPI_COMM_WORLD, status, error)
    END IF
    
    !# ALLOCATE MEMORY ON ROOT
    IF(proc_id .EQ. 0)THEN
      ALLOCATE(bheader(1:counter(1)))
      ALLOCATE(bint(1:counter(2)))
      ALLOCATE(bdp(1:counter(3)))
      ALLOCATE(blogical(1:counter(4)))   
    END IF
    
    ! GATHER:  SEND BUFFERS TO ROOT
    IF(proc_id .EQ. worker_id)THEN  
      CALL MPI_SEND(bheader, counter(1), MPI_INTEGER, 0, tag(2), MPI_COMM_WORLD, error)
      CALL MPI_SEND(bint, counter(2), MPI_INTEGER, 0, tag(3), MPI_COMM_WORLD, error)
      CALL MPI_SEND(bdp, counter(3), MPI_DOUBLE_PRECISION, 0, tag(4), MPI_COMM_WORLD, error)
      CALL MPI_SEND(blogical, counter(4), MPI_LOGICAL, 0, tag(5), MPI_COMM_WORLD, error)
    ELSE IF(proc_id .EQ. 0)THEN 
      CALL MPI_RECV(bheader, counter(1), MPI_INTEGER, worker_id, tag(2), MPI_COMM_WORLD, status, error)
      CALL MPI_RECV(bint, counter(2), MPI_INTEGER, worker_id, tag(3), MPI_COMM_WORLD, status, error)
      CALL MPI_RECV(bdp, counter(3), MPI_DOUBLE_PRECISION, worker_id, tag(4), MPI_COMM_WORLD, status, error)
      CALL MPI_RECV(blogical, counter(4), MPI_LOGICAL, worker_id, tag(5), MPI_COMM_WORLD, status, error)
    END IF
    
  END IF


  DO n = 1, tarr_size