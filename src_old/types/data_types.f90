MODULE data_types
USE kinds

TYPE :: t_data1
INTEGER(KIND=StandardInteger) :: n
REAL(kind=DoubleReal) :: energy_a = 0.0D0
REAL(kind=DoubleReal) :: energy_b = 0.0D0
REAL(kind=DoubleReal), ALLOCATABLE :: stress(:, :)
REAL(kind=DoubleReal), ALLOCATABLE :: center_a(:)
INTEGER(KIND=StandardInteger), ALLOCATABLE :: n_arr_2d(:, :)
INTEGER(KIND=StandardInteger), ALLOCATABLE :: n_arr_1d(:)
REAL(kind=DoubleReal), ALLOCATABLE :: center_b(:)
REAL(kind=DoubleReal) :: center_c(1:3)
REAL(kind=DoubleReal) :: center_d(1:3,1:3)
INTEGER(KIND=StandardInteger) :: n_arr(1:9)
END TYPE t_data1

END MODULE data_types