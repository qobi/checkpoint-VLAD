      function ilog2(l)
      ilog2 = dlog(real(l, 8))/dlog(2.0d0)
      end

      subroutine rotate(theta, x1, x2, x1p, x2p)
      double precision theta, x1, x2, x1p, x2p, c, s
      c = cos(theta)
      s = sin(theta)
      x1p = c*x1-s*x2
      x2p = s*x1+c*x2
      end

      subroutine rot1(theta, n, x)
      double precision theta, x, x1p, x2p
      dimension x(n)
      do k = 1, n-1, 2
         call rotate(theta, x(k), x(k+1), x1p, x2p)
         x(k) = x1p
         x(k+1) = x2p
      end do
      end

      subroutine rot2(theta, n, x)
      double precision theta, x, x1p, x2p
      dimension x(n)
      do k = 2, n-2, 2
         call rotate(theta, x(k), x(k+1), x1p, x2p)
         x(k) = x1p
         x(k+1) = x2p
      end do
      end

      subroutine magsqr(n, x, y)
      double precision x, y
      dimension x(n)
      y = 0.0d0
      do k = 1, n
         y = y+x(k)*x(k)
      end do
      end

      subroutine f(n, x, l, phi, y)
      double precision phi, x, y, x1
      dimension x(n), x1(1000)
      do k = 1, n
         x1(k) = x(k)
      end do
      do i = 1, l
         m = 2**(ilog2(l)-
     +           ilog2(1+int(mod(1013.0d0*3.0d0**phi*real(i, 8),
     +                           real(l, 8)))))
         do j = 1, m
            call magsqr(n, x1, y)
            y = sqrt(y)
            call rot1(1.2d0*y, n, x1)
            call rot2(1.4d0*y, n, x1)
         end do
      end do
      call magsqr(n, x1, y)
      y = y/2.0d0
      end

      program main
      double precision phi, x, xb, y, yb
      dimension x(1000), xb(1000)
      read *, n
      read *, l
      read *, phi
      do k = 1, n
         x(k) = n+1-k
         xb(k) = 0.0d0
      end do
      yb = 1.0d0
      call f(n, x, l, phi, y)
      print *, y
      call f_b(n, x, xb, l, phi, y, yb)
      do k = 1, n
         print *, xb(k)
      end do
      end
