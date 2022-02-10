      function ilog2(l)
      ilog2 = dlog(real(l, 8))/dlog(2.0d0)
      end

      subroutine f(l, x)
      double precision x
      do i = 1, l
         m = 2**(ilog2(l)-
     +           ilog2(1+int(mod(1013.0d0*3.0d0**x*real(i, 8),
     +                           real(l, 8)))))
         print *, m
      end do
      end

      program main
      read *, l
      call f(l, 3.0d0)
      end
