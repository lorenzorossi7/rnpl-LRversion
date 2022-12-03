      program     tpart

      implicit    none

      character*5 R
      parameter ( R = 'tpart' )

      integer     iargc, i4arg

      integer     np, nf

      integer     maxnp
      parameter ( maxnp = 2**20 )
      integer     maxnf
      parameter ( maxnf = 2**3 )
      real*8      u(maxnp*(3+maxnf))

      if( iargc() .ne. 2 ) go to 900
      np = i4arg(1,-1)
      nf = i4arg(2,-1)
      if( np .lt. 1  .or.  nf .lt. 0 ) go to 900
      write(0,*) R//': np = ', np
      write(0,*) R//': nf = ', nf
      if( np .gt. maxnp ) goto 950

      call dxyz(u,u(1+np),u(1+2*np),np)
      call dxyzf(u,np,3+nf)

      stop

 900  continue
         write(0,*) 'usage: '//R//' <np> <nf>'
      stop

 950  continue
         write(0,*) R//': Requested np = ', np, ' is too large'
      stop

      end

      subroutine dxyz(x,y,z,np)
         implicit    none

         real*8      drand48

         integer     np
         real*8      x(np), y(np), z(np)

         integer     i, j

         do i = 1 , np
            x(i) = drand48()
            y(i) = drand48()
            z(i) = drand48()
         end do

         call gft_out_part_xyz('tpart_xyz',-2.0d0,x,y,z,np)

         return

      end

      subroutine dxyzf(u,np,nfp3)
         implicit    none

         real*8      drand48

         integer     np, nfp3
         real*8      u(np,nfp3)

         integer     i, j

         do j = 1 , nfp3
            do i = 1 , np
               u(i,j) = drand48()
            end do
         end do

         call gft_out_part_xyzf('tpart_xyfz',-2.0d0,u,np,nfp3-3)

         return

      end
