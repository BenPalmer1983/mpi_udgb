MODULE data_types
USE kinds

TYPE :: t_data1
LOGICAL :: switch = .FALSE.
INTEGER(KIND=StandardInteger) :: n
REAL(kind=DoubleReal) :: energy_a = 0.0D0
REAL(kind=DoubleReal) :: energy_b = 0.0D0
REAL(kind=DoubleReal), ALLOCATABLE :: stress(:, :)
REAL(kind=DoubleReal), ALLOCATABLE :: center_a(:)
INTEGER(KIND=StandardInteger), ALLOCATABLE :: n_arr_2d(:, :)
INTEGER(KIND=StandardInteger), ALLOCATABLE :: n_arr_1d(:)
REAL(kind=DoubleReal), ALLOCATABLE :: center_b(:)
REAL(kind=DoubleReal), ALLOCATABLE :: center_c(:)
REAL(kind=DoubleReal), ALLOCATABLE :: center_d(:,:)
INTEGER(KIND=StandardInteger), ALLOCATABLE :: n_arr(:)
END TYPE t_data1




TYPE :: t_potential

! ON/OFF
LOGICAL :: f_on = .FALSE.

! TYPE
INTEGER(kind=StandardInteger) :: f_type = 0   ! 1 pair 2 dens 3 emb
INTEGER(kind=StandardInteger) :: a
INTEGER(kind=StandardInteger) :: b = 0
INTEGER(kind=StandardInteger) :: group = 1

REAL(kind=DoubleReal) :: rcut = 6.5D0
INTEGER(kind=StandardInteger) :: zoor = 1



! Analytic
LOGICAL :: a_on = .FALSE.
INTEGER(kind=StandardInteger) :: a_type = 0
REAL(kind=DoubleReal), ALLOCATABLE :: a_params(:)
REAL(kind=DoubleReal) :: a_lower
REAL(kind=DoubleReal) :: a_upper
INTEGER(kind=StandardInteger) :: a_point_count

! POTENTIAL - RAW INPUT
INTEGER(kind=StandardInteger) :: p_count_raw
REAL(kind=DoubleReal), ALLOCATABLE :: points_raw(:,:)

! POTENTIAL - INTERP


REAL(kind=DoubleReal), ALLOCATABLE :: points(:,:)
REAL(kind=DoubleReal) :: px_min
REAL(kind=DoubleReal) :: px_max
REAL(kind=DoubleReal) :: px_d
REAL(kind=DoubleReal) :: px_range


END TYPE t_potential



END MODULE data_types