program main

interface
   subroutine print_array(A, count) BIND(C, name="print_array")
     !DEC$ ATTRIBUTES NO_ARG_CHECK :: A
     !GCC$ ATTRIBUTES NO_ARG_CHECK :: A
     real :: A
     integer, value :: count
   end subroutine
end interface

  integer :: A(10)

  A = [1,2,3,4,5,6,7,8,9,10]

  print *, A
  print *, A(::2)

  call print_array(A, 10)
  call print_array(A(::2), 5)  

end program
