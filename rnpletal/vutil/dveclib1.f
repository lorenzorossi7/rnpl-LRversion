c---------------------------------------------------------------------
c
c     D O U B L E     P R E C I S I O N     V E C T O R
c
c             O P E R A T I O N S   (C O N T .)
c
c---------------------------------------------------------------------
c
      subroutine dvi2q1(v1,v2,n1,irho)
         
         implicit       none

         integer        n1,          irho
         real*8         v1(n1),      v2(*)

         real*8         rhom1 

         integer        i,           j
         
         if( n1 .gt. 0 ) then
            if( irho .gt. 0 ) then
               rhom1 = 1.0d0 / irho
               do i = 1 , n1 - 1
                  do j = 0 , irho - 1
                     v2(irho * (i - 1) + j + 1) = 
     *                  (1.0d0 - j * rhom1) * v1(i) +
     *                  (        j * rhom1) * v1(i + 1)
                  end do
               end do
               v2(irho * (n1 - 1) + 1) = v1(n1)
            end if
         end if

         return

      end

c---------------------------------------------------------------------
c
c     Makes masks ...
c
c---------------------------------------------------------------------

      subroutine dvmask(v,mask,n,ref,tval,fval,op)

         implicit       none

         character*2    u2l_dv
         integer        n
         real*8         v(n),       mask(n)
         real*8         ref,        tval,         fval
         character*2    op

         character*2    lop
         integer        i

         lop = u2l_dv(op)
         if(      lop .eq. 'eq' ) then
            do i = 1 , n 
               if( v(i) .eq. ref ) then
                  mask(i) = tval
               else 
                  mask(i) = fval
               end if
            end do
         else if( lop .eq. 'ne' ) then
            do i = 1 , n 
               if( v(i) .ne. ref ) then
                  mask(i) = tval
               else 
                  mask(i) = fval
               end if
            end do
         else if( lop .eq. 'gt' ) then
            do i = 1 , n 
               if( v(i) .gt. ref ) then
                  mask(i) = tval
               else 
                  mask(i) = fval
               end if
            end do
         else if( lop .eq. 'lt' ) then
            do i = 1 , n 
               if( v(i) .lt. ref ) then
                  mask(i) = tval
               else 
                  mask(i) = fval
               end if
            end do
         else if( lop .eq. 'ge' ) then
            do i = 1 , n 
               if( v(i) .ge. ref ) then
                  mask(i) = tval
               else 
                  mask(i) = fval
               end if
            end do
         else if( lop .eq. 'le' ) then
            do i = 1 , n 
               if( v(i) .le. ref ) then
                  mask(i) = tval
               else 
                  mask(i) = fval
               end if
            end do
         else 
            write(*,*) 'dvmask: Unrecognized map op: '//op
         end if

         return

      end

c---------------------------------------------------------------------
c
c     Local version of utilio routine ...
c
c---------------------------------------------------------------------

      character*(*) function u2l_dv(s)

         implicit        logical (a-z)

         character*(*)   s

         integer         i,    ix

         character*26    uc,   lc
         data            uc / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' /,
     *                   lc / 'abcdefghijklmnopqrstuvwxyz' /

         do i = 1 , len(s)
             ix = index(uc,s(i:i))
            if( ix .ne. 0 ) then
               u2l_dv(i:i) = lc(ix:ix)
            else
               u2l_dv(i:i) = s(i:i)
            end if
         end do

         return

      end

c---------------------------------------------------------------------
c
c     Returns (first encountered) extrema information ...
c
c---------------------------------------------------------------------

      subroutine dvextrema(v,n,vmin,imin,vmax,imax)

         implicit          none

         integer           n
         real*8            v(n)

         real*8            vmin,       vmax
         integer           imin,       imax

         integer           i

         if( n .gt. 0 ) then
            vmin = v(1)
            imin = 1
            vmax = v(1)
            imax = 1
            do i = 2 , n
               if( v(i) .lt. vmin ) then
                  vmin = v(i)
                  imin = i
               end if
               if( v(i) .gt. vmax ) then
                  vmax = v(i)
                  imax = i
               end if
            end do
         else 
            imin = 0
            imax = 0
         end if
      
         return

      end

c---------------------------------------------------------------------
c
c     Vector first difference (non-divided) ...
c
c---------------------------------------------------------------------

      subroutine dvd1(v,dv,n)

         implicit     none
      
         integer      n
         real*8       v(n),       dv(n)

         integer      j
         
         do j =  1 , n - 1
            dv(j) = v(j+1) - v(j)
         end do

         return

      end 

c---------------------------------------------------------------------
c
c     Computes uniform mesh function, also returns mesh interval ...
c
c---------------------------------------------------------------------

      subroutine dvumesh(vmin,vmax,v,n,dv)

         implicit     none
      
         integer      n
         real*8       v(n)
         real*8       vmin,    vmax,    dv

         if(      n .gt. 1 ) then
            dv = (vmax - vmin) / (n - 1)
            call dvramp(v,vmin,dv,n)
            v(n) = vmax
         else if( n .eq. 1 ) then
            v(1) = vmin
            dv = 0.0d0
         end if

         return

      end 

c---------------------------------------------------------------------
c
c     Vector non-uniform first divided difference ...
c
c---------------------------------------------------------------------

      subroutine dvdd1nu(v,x,nin,dvdx,xdvdx,nout)

         implicit     none
      
         integer      nin,         nout
         real*8       v(nin),      x(nin),      dvdx(*),      xdvdx(*)

         integer      j
         real*8       dx 
         
         if( nin .ge. 2 ) then
            nout = nin - 1
            do j =  1 , nout
               xdvdx(j) = 0.5d0 * (x(j) + x(j+1))
               dx       = (x(j+1) - x(j))
               if( dx .ne. 0 ) then
                  dvdx(j) = (v(j+1) - v(j)) / dx
               else
                  dvdx(j) = 0.0d0
               end if
            end do
         else 
            nout = 0
         end if

         return

      end 

c---------------------------------------------------------------------
c 
c     Computes cos(angle) between two vectors ...
c
c---------------------------------------------------------------------

      double precision function dvvcos(v1,v2,n)

         implicit       none

         real*8         dvvdot

         integer        n
         real*8         v1(n),      v2(n)

         real*8         magv1v2

         logical        ltrace
         parameter    ( ltrace = .true. )

         magv1v2 = sqrt(dvvdot(v1,v1,n) * dvvdot(v2,v2,n))
         if( magv1v2 .eq. 0.0d0 ) then
            if( ltrace ) then
               write(*,*) 'dvvcos: 0-length vector encountered.'
            end if
            dvvcos = 0.0
          else
            dvvcos = dvvdot(v1,v2,n) / magv1v2
         end if

         return

      end 

c---------------------------------------------------------------------
c 
c     Computes pearson coefficient between two vectors ...
c
c---------------------------------------------------------------------

      double precision function dvvpears(v1,v2,n)

         implicit       none

         real*8         dvmean

         integer        n
         real*8         v1(n),      v2(n)

         real*8         meanv1,     meanv2,
     *                  sumx,       sumy,        sumxy
         integer        i

         logical        ltrace
         parameter    ( ltrace = .true. )

         meanv1 = dvmean(v1,n)
         meanv2 = dvmean(v2,n)
         sumx  = 0.0d0
         sumy  = 0.0d0
         sumxy = 0.0d0
         do i = 1 , n
            sumx  = sumx  + (v1(i) - meanv1) ** 2
            sumy  = sumy  + (v2(i) - meanv2) ** 2
            sumxy = sumxy + (v1(i) - meanv1) * (v2(i) - meanv2)
         end do
         if( sumx * sumy .eq. 0.0d0 ) then
            if( ltrace ) then
               write(*,*) 'dvvpears: 0-vector encountered.'
            end if
            dvvpears = 0.0d0
         else 
            dvvpears = sumxy / sqrt(sumx * sumy)
         end if

         return

      end 

c---------------------------------------------------------------------
c
c     "Full" linear least squares fitting.  Returns fit, fit 
c     parameters a, b (yfit = a + b x), and goodness of fit 
c     (Pearson's coefficient, r)
c
c---------------------------------------------------------------------

      subroutine dvllsfull(x,y,yfit,n,a,b,r)

         implicit          none

         real*8            clls,     dvvpears

         integer           n
         real*8            x(n),     y(n),    yfit(n)
         real*8            a,        b,       r

         if( clls(x,y,yfit,-n,a,b) .ge. 0.0d0 ) then
            call dvsm(x,b,yfit,n)
            call dvsa(yfit,a,yfit,n)
            r = dvvpears(x,y,n)
         else
            call dvcopy(y,yfit,n)
            r = 0.0d0
         end if

         return

      end 

c-----------------------------------------------------------------------
c
c     History: DVGLMF ...
c
c     Sign-sensitive modifications so that routine works sensibly 
c     on non-positive vectors ...
c
c     Vector generalized mean with floor.
c
c     V2(j) := Signed[max(FLOOR,mean(V1(j-P),...V1(j),...V1(j+P)))]
c
c-----------------------------------------------------------------------
c
      subroutine dvsglmf(v1,v2,n,p,floor)
c
         implicit       none
c
         real*8         dvamax
c
         integer        n
         real*8         v1(n),        v2(n)
         real*8         floor
         integer        p
c
         real*8         itwop1
         integer        i,            j
c
         itwop1 = 1.0d0 / (2 * p + 1)
         if( n .ge. (2 * p + 1) ) then
            call dvls(v2,0.0d0,n)
            do i = p + 1 , n - p
               do j = -p , p
                  v2(i) = v2(i) + v1(i+j)
               end do  
            end do
            do i = p + 1 , n - p
               v2(i) = sign(max(abs(floor),
     *                          abs(v2(i) * itwop1)),v2(i))
            end do
            do i = 1 , p
               v2(i) = v2(p+1)
            end do
            do i = n - p + 1 , n
               v2(i) = v2(n-p)
            end do
         else
            call dvls(v2,sign(max(abs(floor),
     *                            dvamax(v1,n)),v1(1)),n)
         end if
c
         return
c
      end

c-----------------------------------------------------------------------
c
c     Vector absolute value ...
c
c-----------------------------------------------------------------------

      subroutine dvabs(v1,v2,n)
         
         implicit      none

         integer       n
         real*8        v1(n),        v2(n)

         integer       i

         do i = 1 , n
            v2(i) = abs(v1(i))
         end do

         return

      end 

c-----------------------------------------------------------------------
c
c     Vector (log10 - v1_0)
c
c-----------------------------------------------------------------------

      subroutine dvlog10(v1,v2,v1_0,n)
         
         implicit      none

         integer       n
         real*8        v1(n),        v2(n)
         real*8        v1_0

         integer       i

         do i = 1 , n
            if( (v1(i) - v1_0) .le. 0.0d0 ) then
               v2(i) = 0.0d0
            else 
               v2(i) = log10(v1(i) - v1_0)
            end if
         end do

         return

      end 

c-----------------------------------------------------------------------
c
c     Same as dvtwri1 but format supplied ...
c
c-----------------------------------------------------------------------

      subroutine dvtwri1f(v1,v2,v3,n,sfmt,unit)

         implicit      none

         integer       n,        unit
         real*8        v1(n),    v2(n),    v3(n)
         character*(*) sfmt

         integer       j

         do j = 1 , n
            write(unit,fmt=sfmt) v1(j), v2(j), v3(j)
         end do

         return

      end

c-----------------------------------------------------------------------
c
c     Initializes vector on periodic domain to "Gaussian" profile.
c
c-----------------------------------------------------------------------
c
      subroutine dvgauspbc(v,x,v0,x0,xwid,n)
 
         real*8       v(*),    x(*)
         real*8       v0,      xwid,    x0

         real*8       arg 
         real*8       argmax 
         parameter  ( argmax = 169.0d0 )

         real         xmin,    xmax,    delx
         integer      i,     n
 
         xmin = x(2)
         xmax = x(n)
         do i = 1 , n
            delx = min(abs(x0 - x(i)),xmax - xmin - abs(x0 - x(i)))
            arg = (delx / xwid) ** 2
            if( arg .lt. argmax ) then
               v(i) = v0 * exp(-arg)
            else
               v(i) = 0.0d0
            end if
         end do

         return
 
      end

c-----------------------------------------------------------------------
c
c     Produces FORTRAN-canonical pth-order periodic extension of 
c     input vector ...
c
c-----------------------------------------------------------------------

      subroutine dvpbcextend(vin,vout,nin,nout,p)

         implicit        none

         integer         nin,       nout,       p
         real*8          vin(nin),  vout(*)

         integer         j

         logical         ltrace
         parameter     ( ltrace = .true. )

         if( p .ge. 0 ) then
            nout = nin + 2 * p
            do j = 1 , nin
               vout(p + j) = vin(j)
            end do

            do j = 1 , p
               vout(j)           = vout(nin + j)
               vout(nin + p + j) = vout(p + j)
            end do
         else 
            if( ltrace ) then
               write(*,*) 'dvpbcextend: Warning ... p < 0. '//
     *                    'Setting nout = 0'
            end if
            nout = 0
         end if

         return

      end

c-----------------------------------------------------------------------
c
c     Sets p-th order canonical periodic boundary conditions ...
c
c-----------------------------------------------------------------------

      subroutine dvpbcset(v,n,p)

         implicit        none

         integer         n,         p
         real*8          v(n)

         integer         j,         nbase

         logical         ltrace
         parameter     ( ltrace = .true. )

         nbase = n - 2 * p
         if( p .ge. 0  .and.  nbase .gt. 0 ) then
            do j = 1 , p
               v(j)             = v(nbase + j)
               v(nbase + p + j) = v(p + j)
            end do
         end if

         return

      end

c-----------------------------------------------------------------------
c
c     Predicate:  Is vector in canonical p-th order periodic form?
c
c-----------------------------------------------------------------------

      logical function haspbcp(v,n,p)

         implicit      none

         integer       n,        p
         real*8        v(n)

         integer       nbase,    j

         haspbcp = .true. 
         nbase = n - 2 * p
         if( nbase .gt. 0  .and.  p .ge. 0 ) then
            do j = 1 , p
               haspbcp = haspbcp .and. ( v(j) .eq. v(nbase + j) )  
     *                   .and. ( v(nbase + p + j) .eq.  v(p + j) )
            end do
         else
            haspbcp = .false.
         end if

         return

      end 

c-----------------------------------------------------------------------
c
c     Deletes FORTRAN-canonical pth-order periodic extension from
c     input vector ...
c
c-----------------------------------------------------------------------

      subroutine dvpbcdelete(vin,vout,nin,nout,p)

         implicit        none

         logical         haspbcp

         integer         nin,       nout,       p
         real*8          vin(nin),  vout(*)

         logical         ltrace
         parameter     ( ltrace = .true. )

         if( p .ge. 0 ) then
            nout = nin - 2 * p
            call dvcopy(vin(p+1),vout,nout)
         else 
            if( ltrace ) then
               write(*,*) 'dvpbcdelete: Warning ... p < 0. '//
     *                    'Setting nout = 0'
            end if
            nout = 0
         end if

         return

      end

c-----------------------------------------------------------------------
c
c    Converts from one periodic form to another, requires temporary 
c    storage for base vector ...
c
c    Doesn't ensure that vin is actually pin-periodic.
c
c-----------------------------------------------------------------------

      subroutine dvpbcconvert(vin,vout,vtemp,nin,nout,pin,pout)

         implicit        none

         logical         haspbcp

         integer         nin,       nout,       pin,    pout
         real*8          vin(nin),  vout(*),    vtemp(*)

         integer         ntemp

         logical         ltrace
         parameter     ( ltrace = .true. )

         if( pin .ge. 0  .and.  pout .ge. 0 ) then
            call dvpbcdelete(vin,vtemp,nin,ntemp,pin)
            call dvpbcextend(vtemp,vout,ntemp,nout,pout)
         else 
            if( ltrace ) then
               write(*,*) 'dvpbcconvert: Warning ... pin/pout < 0. '//
     *                    'Setting nout = 0'
            end if
            nout = 0
         end if

         return

      end

c---------------------------------------------------------------------
c
c     Returns (first encountered) extrema information with constraints
c     on search intervals ...
c
c---------------------------------------------------------------------

      subroutine dvextremacons(f,x,n,minxmin,maxxmin,minxmax,maxxmax,
     *                         fmin,imin,fmax,imax)

         implicit          none

         integer           n
         real*8            f(n),       x(n)

         real*8            minxmin,    maxxmin,    minxmax,    maxxmax,
     *                     fmin,       fmax
         integer           imin,       imax

         integer           i

         imin = 0
         imax = 0
         if( n .gt. 0 ) then
            do i = 1 , n
               if( minxmin .le. x(i)  .and.  x(i) .le. maxxmin ) then
                  if( imin .eq. 0 ) then
                     fmin = f(i)
                     imin = i
                  else if( f(i) .lt. fmin ) then
                     fmin = f(i)
                     imin = i
                  end if
               end if
               if( minxmax .le. x(i)  .and.  x(i) .le. maxxmax ) then
                  if( imax .eq. 0 ) then
                     fmax = f(i)
                     imax = i
                  else if( f(i) .gt. fmax ) then
                     fmax = f(i)
                     imax = i
                  end if
               end if
            end do
         end if
      
         return

      end

c---------------------------------------------------------------------
c     Fits vy(vx) to  a + b * x^2  using least squares ...
c---------------------------------------------------------------------

      subroutine dvqfit(vx,vy,n,a,b)
         implicit     none

         integer      n
         real*8       vx(n),     vy(n)
         real*8       a,   b

         real*8       x,   x2,   x4,   x2y,    y,    disc
         integer      j

         a = 0.0d0
         b = 0.0d0

         if( n .ge. 2 ) then
            x   = 0.0d0
            x2  = 0.0d0
            x2y = 0.0d0
            x4  = 0.0d0
            y   = 0.0d0
            do j = 1 , n
               x   = x + vx(j)
               x2  = x2 + vx(j)**2
               x2y = x2y + vx(j)**2 * vy(j)
               x4  = x4 + vx(j)**4
               y   = y + vy(j)
            end do
            disc = n * x4 - x2 * x2
            if( disc .eq. 0.0d0 ) then
               write(0,*) 'dvqfit: Singular system.'
            else
               a =  (-x2 * x2y + x4 * y) / disc
               b =  (  n * x2y - x2 * y)  / disc
            end if
         end if

         return
      end
c
c-----------------------------------------------------------------------
c
c      History: DVINQN, but uses fuzzy equal ...
c
c     (NINTRP-1)th order interpolation from one set of values to another
c     (Both sets must be in ascending order.)
c
c-----------------------------------------------------------------------
c
      subroutine dvinqn_fuzz(v,x,vbar,xbar,n,nbar,vs,vf,nintrp,fuzz)
c
         implicit     none
c
         real*8       flipn
         integer      llogic
         logical      leven
c
         integer      n, nbar, nintrp
         real*8       v(n), vbar(nbar), x(n), xbar(nbar)
         real*8       fuzz
c
         real*8       vs, vf
         integer      i, j, jbar

         logical      ltrace
         parameter  ( ltrace = .false. )
c
         if( ltrace ) then
            WRITE(*,*) '>>> dvinqn:: n',n
            CALL DVPDMP(X,V,N,'V(X)',6)
         end if
c
         jbar = 1
         j = 1
 100     continue
         if( jbar .gt. nbar ) go to 200
            if( xbar(jbar) .lt. x(j) ) then
               if( j .eq. 1 ) then
                  vbar(jbar) = vs
               else
                  if( leven(nintrp) ) then
                     i = min(n-nintrp+1,max(1,j-(nintrp/2)))
                  else
                     i = min(n-nintrp+1,max(1,j-(nintrp+1)/2 +
     *                       llogic(xbar(jbar) .gt.
     *                             0.5d0 * (x(j) + x(j-1)))))
                  end if
                  vbar(jbar) = flipn(xbar(jbar),x(i),v(i),nintrp)
               end if
               jbar = jbar + 1
            else if( abs(xbar(jbar) -  x(j)) .le. fuzz ) then
               vbar(jbar) = v(j)
               jbar = jbar + 1
            else
               if( j .eq. n ) then
                  vbar(jbar) = vf
                  jbar = jbar + 1
               else
                  j = j + 1
               end if
            end if
         go to 100
c
 200     continue
c
         if( ltrace ) then
            WRITE(*,*) '>>> dvinqn:: nbar',nbar
            CALL DVPDMP(XBAR,VBAR,NBAR,'VBAR(XBAR)',6)
         end if

         return
c
      end

c---------------------------------------------------------------------
c
c     Vector-Max(abs(vector),floor) divide ... 
c
c---------------------------------------------------------------------

      subroutine dvvadf(v1,v2,v3,floor,nr)

         implicit       none

         integer        nr
         real*8         v1(nr),      v2(nr),      v3(nr)
         real*8         floor

         integer        j

         if( floor .le. 0.0d0 ) then
            write(0,*) 'dvvadf: floor <= 0'
            return
         end if

         do j = 1 , nr
            v3(j) = v1(j) / max(abs(v2(j)),floor)
         end do

         return

      end

c-----------------------------------------------------------------------
c
c     Mid--point first integral with reset whenever sense of 
c     dx changes and user supplied initial value
c
c-----------------------------------------------------------------------

      subroutine dvmint_dx_sense(y,x,res,n,res0)

         implicit     none

         logical      dx_state_changed
         integer      n
         real*8       y(n),      x(n),        res(n)
         real*8       res0
         
         integer      j

         if( n .le. 1 ) return

         res(1) = res0
         do j = 2 , n
            if( dx_state_changed(x(j) - x(j-1)) ) then
               res(j) = res0
            else 
               res(j) = res(j-1) + 0.5d0 * (x(j) - x(j-1)) * 
     *                                     (y(j) + y(j-1))
            end if
         end do

         return

      end

c-----------------------------------------------------------------------
c
c     Reverse mid--point first integral with reset whenever sense of 
c     dx changes and user supplied initial value
c
c-----------------------------------------------------------------------

      subroutine dvrmint_dx_sense(y,x,res,n,resn)

         implicit     none

         logical      dx_state_changed
         integer      n
         real*8       y(n),      x(n),        res(n)
         real*8       resn
         
         integer      j

         if( n .le. 1 ) return

         res(n) = resn
         do j = n - 1 , 1 , -1
            if( dx_state_changed(x(j+1) - x(j)) ) then
               res(j) = resn
            else 
               res(j) = res(j+1) - 0.5d0 * (x(j+1) - x(j)) *
     *                                     (y(j) + y(j+1))

            end if
         end do

         return

      end

c-----------------------------------------------------------------------
c     f77 implementation of dx_state() routines (~/nvs/src/mplot.[ch)
c     Used to be in libutilmath.a, but better to be here since 
c     referenced by dv*.f source code
c-----------------------------------------------------------------------

      subroutine dx_state_reset()
         implicit     none
         logical      dx_state_0
         logical      rc

         rc = dx_state_0(0,0.0d0)

         return
      end 

      logical function dx_state_changed(new_dx)
         implicit     none
         logical      dx_state_0
         real*8       new_dx
         logical      rc

         dx_state_changed = dx_state_0(1,new_dx)

         return
      end 

      logical function dx_state_0(opcode,new_dx)
         implicit     none

         integer      opcode
         real*8       new_dx

         integer      reset
         parameter  ( reset = 0 )
         integer      query_state_changed
         parameter  ( query_state_changed = 1 )

         integer      dx_state

         integer      dx_pos,      dx_neg,       dx_undef
         parameter  ( dx_pos = 1,  dx_neg = -1,  dx_undef = 0 )

         save         dx_state
        
         dx_state_0 = .false.
         if(      opcode .eq. reset               ) then
            dx_state = dx_undef
         else if( opcode .eq. query_state_changed ) then
            if(      dx_state .eq. dx_undef ) then
               if(      new_dx .gt. 0.0d0 ) then
                  dx_state = dx_pos
               else if( new_dx .lt. 0.0d0 ) then
                  dx_state = dx_neg
               end if
            else if( dx_state .eq. dx_pos ) then
               if(      new_dx .gt. 0.0d0 ) then
               else if( new_dx .lt. 0.0d0 ) then
                  dx_state = dx_undef
                  dx_state_0 = .true.
               end if
            else if( dx_state .eq. dx_neg ) then
               if(      new_dx .gt. 0.0d0 ) then
                  dx_state = dx_undef
                  dx_state_0 = .true.
               else if( new_dx .lt. 0.0d0 ) then
               end if
            end if
         else 
            write(0,*) 'dx_state_0: Unrecognized opcode: ', opcode
         end if 
         
         return
      end 

c---------------------------------------------------------------------------
c
c     1-dimensional 2:1 half-weighted restricion ...
c
c---------------------------------------------------------------------------

      subroutine dvrshw(uf,uc,nxc)
 
         implicit       none

         integer        nxc
         real*8         uf(2 * nxc - 1),
     *                  uc(nxc)
 
         integer        ic,       if
 
         do ic = 2 , nxc - 1
            if = 2 * ic - 1
            uc(ic) = 0.25d0 * (uf(if-1) + 2.0d0 * uf(if) + uf(if+1))
         end do
         uc(1)   = uf(1)
         uc(nxc) = uf(2 * nxc - 1)
 
         return
 
      end

c---------------------------------------------------------------------------
c
c     1-dimensional 2:1 linear interpolation ...
c
c---------------------------------------------------------------------------

      subroutine dvlint(uc,uf,nxc)
 
         implicit       none

         integer        nxc
         real*8         uf(2 * nxc - 1),
     *                  uc(nxc)
 
         integer        ic,       if
 
         do ic = 1 , nxc
            uf(2 * ic - 1) = uc(ic)
         end do
         do if = 2 , 2 * (nxc - 1) , 2
            uf(if) = 0.5d0 * (uf(if-1) + uf(if+1))
         end do
         
         return
 
      end

c---------------------------------------------------------------------------
c
c     Vector load boundary with scalar ...
c
c---------------------------------------------------------------------------

      subroutine dvlbs(v,s,n)

         implicit       none

         integer        n
         real*8         v(*)
         real*8         s

         v(1) = s
         v(n) = s

         return

      end

c---------------------------------------------------------------------------
c
c     Double precision sawtooth profile ... vector split into two 
c     equal length halves: the first ramps from 'v1' to 'vmid', the 
c     second from 'vmid' to 'vn' ...
c
c---------------------------------------------------------------------------

      subroutine dvsaw(v,n,v1,vmid,vn)

         implicit       none

         integer        n
         real*8         v(*)
         real*8         v1,      vmid,     vn
         real*8         tv1,               tvn

         if( n .gt. 0 ) then
            tv1 = v1
            tvn = vn
            if( mod(n,2) .eq. 0 ) then
               call dvumsh(v(1),n/2,v1,vmid)
               call dvumsh(v(n/2+1),n/2,vmid,vn)
            else
               call dvumsh(v(1),(n+1)/2,v1,vmid)
               call dvumsh(v((n+1)/2),(n+1)/2,vmid,vn)
            end if
            v(1) = tv1
            v(n) = tvn
         end if

         return

      end

c---------------------------------------------------------------------------
c
c     Returns x-coordinate of first 0-crossing in data y using linear 
c     interpolation.  Also returns last grid indexed examined so that 
c     routine can be used with dv0x below.  0-crossing *only* detected
c     through change in sign of y.
c
c---------------------------------------------------------------------------

      double precision function fdvx0(x,y,n,jst,jx)

         implicit       none

         integer        n
         real*8         x(n),     y(n)
         integer        jst,      jx

         integer        j
         logical        ltrace 
         parameter    ( ltrace = .false. )

         jx = 0
         fdvx0 = 0.0d0

         if( (n - jst + 1) .lt. 1 ) return

         if( (n - jst + 1) .eq. 1 ) then
            if( y(jst) .eq. 0.0d0 ) then
               jx = jst
               fdvx0 = x(jst)
            end if
            return
         end if

         do j = jst , n - 1
            if( x(j+1) .le. x(j) ) return
         end do
         do j = jst , n - 1
            if( y(j) * y(j+1) .le. 0.0d0 ) then
               if( y(j) .eq. 0.0d0 ) then
                  fdvx0 = x(j)
                  jx = j
                  return
               end if
               if( y(j+1) .eq. 0.0d0 ) then
                  fdvx0 = x(j+1)
                  jx = j + 1
                  return
               end if
               fdvx0 = x(j) - (x(j+1) - x(j)) * y(j) / (y(j+1) - y(j))
               jx = j
               if( ltrace ) then
                  write(0,*) 'fdvx0: Returning ', fdvx0, jx
               end if
               return
            end if
         end do
       
         return

      end

c---------------------------------------------------------------------------
c
c     Returns x-coordinates of all 0-crossings of data in y by calling 
c     'fdvx0' repeatedly.
c
c---------------------------------------------------------------------------
 
      subroutine dvx0(x,y,n,x0,n0)

         implicit       none

         real*8         fdvx0

         integer        n,       n0
         real*8         x(n),    y(n),    x0(n)

         real*8         lx0
         integer        jx,      jst

         n0 = 0   
         if( n .lt. 2 ) return
         jst = 1
100      continue
            lx0 = fdvx0(x,y,n,jst,jx)
            if( jx .eq. 0 ) return
            n0 = n0 + 1
            x0(n0) = lx0
            jst = jx + 1
         go to 100

      end

c---------------------------------------------------------------------------
c
c     Returns SUM(V1[j]*V2[j]) ...
c 
c---------------------------------------------------------------------------
      double precision function dvvsum(v1,v2,n)
         implicit      none

         integer       n
         real*8        v1(n),      v2(n)
         integer       i

         dvvsum = 0.0d0
         do i = 1 , n
            dvvsum = dvvsum + v1(i) * v2(i)
         end do

         return
      end 

c---------------------------------------------------------------------------
c
c     Double precision vector-vector copy with stride ...
c 
c---------------------------------------------------------------------------
      subroutine dvstridecopy(v1,v2,inc1,inc2,ncopy)
         implicit      none

         integer       inc1,       inc2,     ncopy
         real*8        v1(*),      v2(*)
         integer       i 

         do i = 1 , ncopy
            v2((i-1)*inc2 + 1) = v1((i-1)*inc1 + 1)
         end do

      end 

c---------------------------------------------------------------------------
c
c     Double precision x-component of projective projection of c2
c     to s2.
c 
c---------------------------------------------------------------------------
      subroutine dvc2tos2x(cx,cy,x,n)
         implicit      none
      
         integer       n
         real*8        cx(n),      cy(n),      x(n)
         integer       j

         do j = 1 , n
            x(j) =  cx(j) / (1.0d0 +  cx(j)*cx(j) + cy(j)*cy(j))
         end do

         return

      end

c---------------------------------------------------------------------------
c
c     Double precision y-component of projective projection of c2
c     to s2.
c 
c---------------------------------------------------------------------------
      subroutine dvc2tos2y(cx,cy,y,n)
         implicit      none
      
         integer       n
         real*8        cx(n),      cy(n),     y(n)
         integer       j

         do j = 1 , n
            y(j) =  cy(j) / (1.0d0 +  cx(j)*cx(j) + cy(j)*cy(j))
         end do

         return

      end

c---------------------------------------------------------------------------
c
c     Double precision z-component of projective projection of c2
c     to s2.
c 
c---------------------------------------------------------------------------
      subroutine dvc2tos2z(cx,cy,z,n)
         implicit      none
      
         integer       n
         real*8        cx(n),      cy(n),     z(n)
         integer       j

         do j = 1 , n
            z(j) = (cx(j)*cx(j) + cy(j)*cy(j)) /
     &             (1.0d0 +  cx(j)*cx(j) + cy(j)*cy(j))
         end do

         return

      end

c---------------------------------------------------------------------------
c     Returns +1, -1, 0 according to whether v is nondecreasing, 
c     nonincreasing or otherwise, respectively.
c---------------------------------------------------------------------------
      integer function idvsortstate(v,n)
         implicit      none

         integer       n
         real*8        v(*)

         integer       j

         if(       n .eq. 0 ) then
            idvsortstate = 1
         else if ( n .eq. 1 ) then
            idvsortstate = 1
         else 
            if( v(2) .ge. v(1) ) then
               idvsortstate = 1
               do j = 3 , n
                  if( v(j) .lt. v(j-1) ) then
                     idvsortstate = 0
                     return
                  end if
               end do
            else 
               idvsortstate = -1
               do j = 3 , n
                  if( v(j) .gt. v(j-1) ) then
                     idvsortstate = 0
                     return
                  end if
               end do
            end if
         end if

         return

      end

c---------------------------------------------------------------------------
c     Evaluates "quadratic fit residual".
c---------------------------------------------------------------------------
      real*8 function dvqfitres(v,n)
         implicit     none

         integer      n
         real*8       v(n)

         if( n .ge. 3 ) then
            dvqfitres = (3.0d0 * v(1) - 4.0d0 * v(2) + v(3)) / 3.0d0
         else 
            dvqfitres = 0.0d0
         end if

         return   
      end

c-----------------------------------------------------------------------
c     Computes generalized tanh-kink profile v(x(i) , i = 1 , nx)
c-----------------------------------------------------------------------
      subroutine dvgtanhkink(v,x,nx,vmin,vmax,x0,delta,p)
         implicit none
      
         integer     nx
         real*8      v(nx), x(nx)
         real*8      vmin, vmax, x0, delta, p

         integer     i

         do i = 1 , nx
            v(i) = (vmin - vmax) * 
     &             (0.5d0 * (1.0d0 - tanh((x(i) - x0) / delta)))**p +
     &             vmax
         end do

         return
      end 

c---------------------------------------------------------------------
c     Computes v2 = v1 * x / (1 - x)
c---------------------------------------------------------------------
      subroutine dvyxby1mx(v1,x,v2,v2def,n)

         implicit        none

         integer         n
         real*8          v1(n),         x(n),
     &                   v2(n)
         real*8          v2def

         integer         i

         if( n .ge. 1 ) then
            do i = 1 , n
               if( x(i) .ne. 1.0d0 ) then
                  v2(i) = v1(i) * x(i) / (1.0d0 - x(i))
               else
                  v2(i) = v2def
               end if
            end do
         end if

         return

      end

c---------------------------------------------------------------------
c     Computes 1st undivided difference of v, returns in values dv,
c     dv(n) arbitrarily defined to be 0.
c---------------------------------------------------------------------
      subroutine dvdv(v,dv,n)

         implicit        none

         integer         n
         real*8          v(n),          dv(n)

         integer         i

         if( n .gt. 0 ) then
            do i = 1 , n - 1
               dv(i) = v(i+1) - v(i)
            end do
            dv(n) = 0.0d0 
         end if

         return

      end

c---------------------------------------------------------------------
c     Computes kth undivided difference of v, returns in values dkv,
c     dnv(n-j), j=0,..k-1, arbitrarily defined to be 0.  Needs 
c     work array.
c---------------------------------------------------------------------
      subroutine dvdkv(v,dkv,work,n,k)

         implicit        none

         integer         n,             k
         real*8          v(n),          dkv(n),        work(n)

         integer         i,             kloc

         if( n .gt. 0  .and. k .gt. 0 ) then
            kloc = min(k,n-1)
            call dvcopy(v,work,n)
            do i = 0 , kloc - 1
               call dvdv(work,dkv,n-i)
               call dvcopy(dkv,work,n)
            end do
         end if

         return

      end

c-----------------------------------------------------------------------
c     Finds minima/maxima of v(x(i)) such that
c
c         v(i) <= >= v(i+j), j = -ncomp .. ncomp
c-----------------------------------------------------------------------
      subroutine dvminima(v,x,n, vmin,xmin,ixmin,nmin, ncomp)
         implicit     none

         integer      n, nmin, ncomp
         real*8       v(n), x(n), vmin(*), xmin(*)
         integer      ixmin(*)

         integer      k
         
         logical      ismin

         integer      i, j

         nmin = 0
         if( n .lt. 1 ) then
            return
         end if
         if( ncomp .lt. 1 ) then
            return
         end if

         do i = 1 , n
            ismin = .true.
            do j = max(i-ncomp,1) , min(i+ncomp,n)
               ismin = ismin .and. (v(i) .le. v(j))
            end do
            if( ismin ) then
               nmin = nmin + 1
               vmin(nmin) = v(i)
               xmin(nmin) = x(i)
               ixmin(nmin) = i
            end if
         end do
               
         return

      end

      subroutine dvmaxima(v,x,n, vmax,xmax,ixmax,nmax, ncomp)
         implicit     none

         integer      n, nmax, ncomp
         real*8       v(n), x(n), vmax(*), xmax(*)
         integer      ixmax(*)

         integer      k
         
         logical      ismax

         integer      i, j

         nmax = 0
         if( n .lt. 1 ) then
            return
         end if
         if( ncomp .lt. 1 ) then
            return
         end if

         do i = 1 , n
            ismax = .true.
            do j = max(i-ncomp,1) , min(i+ncomp,n)
               ismax = ismax .and. (v(i) .ge. v(j))
            end do
            if( ismax ) then
               nmax = nmax + 1
               vmax(nmax) = v(i)
               xmax(nmax) = x(i)
               ixmax(nmax) = i
            end if
         end do
               
         return

      end
