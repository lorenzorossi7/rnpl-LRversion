c-----------------------------------------------------------------------
c     Creates .segdat file from data in server window ... 
c-----------------------------------------------------------------------

      program       vs2seg

      integer       indlnb,      vsgxynall,      iargc,
     *              getu
      character*16  xxspec

      character*64  name

      integer       max_xy
      parameter   ( max_xy = 2 000 000 )

      integer       max_ngroup
      parameter   ( max_ngroup = 500 000 )
      real*8        x(max_xy),   y(max_xy),      time(max_ngroup) 
      integer       n(max_ngroup)

      integer       nf
      parameter   ( nf = 1 )
      character*16  fname
      character*24  segname

      integer       u

      integer       it,           j,        jj

      logical       ltrace
      parameter   ( ltrace = .false. )

      if( iargc() .lt. 1 ) go to 900
      call sload(name,' ')
      call getarg(1,name)

      nindex_out = vsgxynall(name,x,y,n,time)
      if( nindex_out .gt. 0 ) then
         call sload(fname,' ')
         fname = xxspec(name)
         if( ltrace ) then
            write(0,*) 'vs2seg: vsgxynall(...) returns ', nindex_out
            write(0,*) 'vs2seg: Will name/label with <'// 
     *                 fname(1:indlnb(fname))//'>'
         end if
         call sload(segname,' ')
         segname = fname(1:indlnb(fname))//'.segdat'
         u = getu()
         open(u,file=segname(1:indlnb(segname)),form='unformatted',
     *        status='new',err=950)
         write(u) nf
         write(u) fname
         j = 1
         do it = 1 , nindex_out
            write(u) time(it),  n(it)
            write(u) ( x(jj) , jj = j , j + n(it) - 1 )
            write(u) ( y(jj) , jj = j , j + n(it) - 1 )
            j = j + n(it)
         end do
         close(u)
         write(*,*) segname(1:indlnb(segname))
      else 
         write(0,*) 'vs2seg: Can not extract data from window <'//
     *              name(1:indlnb(name))//'>'
      end if

      stop

900   continue
         write(0,*) 'usage: vs2seg <window name>'
      stop

950   continue
         write(0,*) 'vs2seg: Error opening (NEW) '//
     *              segname(1:indlnb(segname))
      stop

      end
