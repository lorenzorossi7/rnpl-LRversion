c------------------------------------------------------------------
c     Utility routines for handling CORNELL 1-d data format
c     (Greg Cook, Mark Scheel).  
c------------------------------------------------------------------
		subroutine scanempdat(fname,vt,vnr,vnf,nt,maxnt,fast)
			implicit        none
			integer         getu,       indlnb

			character*(*)   fname
			integer         nt,         maxnt,        fast
			real*8          vt(maxnt),  vnr(maxnt),   vnf(maxnt)

			integer         u,          rc,           ifcn,
     &                   recno

			nt = 0
			u = getu()
			open(u,file=fname(1:indlnb(fname)),status='old',
     &        form='unformatted',iostat=rc)
			if( rc .ne. 0 ) then
				write(0,1000) 'opening', fname(1:indlnb(fname))
1000        format(' scanempdat: Error ',a,' <',a,'> ')
			end if

			recno = 0
 100     continue 
				recno = recno + 1
				read(u,end=200,iostat=rc)  
     &           vt(nt+1), vnr(nt+1), vnf(nt+1)
				if( rc .ne. 0 ) go to 300
				nt = nt + 1
				do ifcn = 1 , vnf(nt)
					recno = recno + 1
					read(u)
				end do
			go to 100

 200     continue
			close(u)
			return

 300     continue
				write(0,1000) 'reading', fname(1:indlnb(fname))
				write(0,*) 'scanempdat: Record ', recno
			close(u)

			return
		end 
