/* Nested chebyshev fitting ... */

#include "nestcf.h"

PFNODE make_FNODE(int i_min, int i_max, PFNODE next) {
   PFNODE      new;

   if( (new = (PFNODE) malloc(sizeof(FNODE)))  ) {
      new->i_min = i_min;
      new->i_max = i_max;
      new->opt_nc = -1;
      new->c = (DVEC) NULL;
      new->fit = -1.0;
      new->next = next;
   } else {
      fprintf(stderr,"make_FNODE: malloc(%d) failure.\n",(int) sizeof(FNODE));
   }
   return new;
}

void   dump_FNODE_list(char *mess,PFNODE head) {
   PFNODE       p;
   int          i;

   printf("\ndump_FNODE_list: %s\n",mess);
   for( p = head, i = 1; p != (PFNODE) NULL; p = p->next, i++ ) {
      printf("Node %3d: %4d .. %4d\n",i,p->i_min,p->i_max);
      if( p->c ) {
         dvdump(p->c,(p->opt_nc)+1,"Cheby coeffs.");
         printf("Fit: %12.e\n",p->fit);
      }
   }
}

void   dump_FNODE_list_brief(char *mess,PFNODE head,PFNODE curr) {
   PFNODE       p;
   int          i;

   printf("\ndump_FNODE_list: %s\n",mess);
   for( p = head, i = 1; p != (PFNODE) NULL; p = p->next, i++ ) {
      if( p == curr ) {
         printf("Node %3d: %4d .. %4d (%d)*\n",
                i,p->i_min,p->i_max,npts_FNODE(p));
      } else {
         printf("Node %3d: %4d .. %4d (%d)\n",
                i,p->i_min,p->i_max,npts_FNODE(p));
      }
   }
}

PFNODE split_FNODE(PFNODE old) {
   PFNODE       new;
   int          new_i;

   new_i = (old->i_min + old->i_max + 1) / 2;
   if( new_i == old->i_min || new_i == old->i_max ) {
      fprintf(stderr,"split_FNODE: Can't split node %4d .. %4d.\n",
              old->i_min,old->i_max);
      return (PFNODE) NULL;
   } else {
      new = make_FNODE(new_i,old->i_max,old->next);
      old->i_max = new_i;
      old->next = new;
      return old;
   }
}

/* Modifications: Minimum length of fit segment, no longer fails if fit 
   criteria not achieved ... */

int npts_FNODE(PFNODE p) {
   return p->i_max - p->i_min + 1;
}

int nestcf(DVEC x, DVEC f, int n, int min_nc, int max_nc, double fit_crit,
           DVEC xseg, int *pnseg, DMAT c, IVEC nc) {
   PFNODE    head,    p;

   double    fnrm;
   int       iseg,  nseg;
   int       ltrace = OFF;

   int       Min_npts = 8;
   int       n_fit_failed = 0;

   if( n < 4 ) return 0;

   head = make_FNODE(0,n-1,(PFNODE) NULL);
   if( ltrace ) {
      dump_FNODE_list("Initial list",head);
   }
   fnrm = dvnrm2_(f,&n);
   for( p = head; p != NULL; ) {
      int    tn, ifit;
      double f77_one = 1.0, f77_xmin, f77_xmax;
      DVEC   xw, fw, cw, fit;

      tn = p->i_max - p->i_min + 1;
      if( ltrace ) {
         printf("nestcf: Before dvchmcft: %d points.\n",tn);
      }
      if( !(xw = make_DVEC(tn)) || !(fw = make_DVEC(tn)) ||
          !(cw = make_DVEC(max_nc+1)) || !(fit = make_DVEC(max_nc+1)) ) {
         fprintf(stderr,"nestcf: make_DVEC failure.\n");
         return 0;
      }
      ifit = dvchmcft_(x+(p->i_min),f+(p->i_min),xw,fw,&tn,cw,&min_nc,&max_nc,
                       &f77_xmin,&f77_xmax,&fnrm,&f77_one,&f77_one,fit);
      if( ifit < 0 ) {
         fprintf(stderr,"nestcf: dvchmcft_ failed ... data follows.\n");
         dvprint(x+(p->i_min),tn,"x");
         dvprint(f+(p->i_min),tn,"f");
         return 0;
      }

      if( ltrace ) {
         printf("nestcf: dvchmcft returns %d\n",ifit);
         dvdump(fit,max_nc,"fits from dvchmcft.\n");  
      }
      if( (fit[ifit] <= fit_crit) || (npts_FNODE(p) <= Min_npts) ) {
         p->fit = fit[ifit];
         p->opt_nc = ifit;
         p->c = Dvcopy(cw,ifit+1);
         p = p->next;
      } else {
         p = split_FNODE(p);
      }
      free_DVEC(xw); free_DVEC(fw); free_DVEC(cw); free_DVEC(fit);
      if( ltrace ) {
         dump_FNODE_list_brief("In loop",head,p);
      }
   }
   for( p = head, nseg = 0; p != NULL; p = p->next ) {
      if( p->fit < 0.0 || p->fit > fit_crit ) {
         ++n_fit_failed;
      }
      ++nseg;
   }
   if( n_fit_failed > 0 ) {
      printf("nestcf: Warning: Fit criteria not achieved on %d segments.\n",
             n_fit_failed);
   }
   if( ltrace ) {
      printf("nestcf: %d segments.\n",nseg);
   }
   for( iseg = 0, p = head; iseg < nseg; iseg++, p = p->next ) {
      c[iseg] = p->c;
      nc[iseg] = p->opt_nc;
      xseg[iseg] = x[p->i_min];
      if( iseg == nseg - 1 ) {
         xseg[nseg] = x[p->i_max];
      }
   }
   *pnseg = nseg; 
   return 1;
}

int old_nestcf(DVEC x, DVEC f, int n, int min_nc, int max_nc, double fit_crit,
               DVEC xseg, int *pnseg, DMAT c, IVEC nc) {
   PFNODE    head,    p;

   double    fnrm;
   int       iseg,  nseg;
   int       ltrace = OFF;

   if( n < 4 ) return 0;

   head = make_FNODE(0,n-1,(PFNODE) NULL);
   if( ltrace ) {
      dump_FNODE_list("Initial list",head);
   }
   fnrm = dvnrm2_(f,&n);
   for( p = head; p != NULL; ) {
      int    tn, ifit;
      double f77_one = 1.0, f77_xmin, f77_xmax;
      DVEC   xw, fw, cw, fit;

      tn = p->i_max - p->i_min + 1;
      if( !(xw = make_DVEC(tn)) || !(fw = make_DVEC(tn)) ||
          !(cw = make_DVEC(max_nc+1)) || !(fit = make_DVEC(max_nc+1)) ) {
         fprintf(stderr,"nestcf: make_DVEC failure.\n");
         return 0;
      }
      ifit = dvchmcft_(x+(p->i_min),f+(p->i_min),xw,fw,&tn,cw,&min_nc,&max_nc,
                       &f77_xmin,&f77_xmax,&fnrm,&f77_one,&f77_one,fit);
      if( ifit <= 0 ) return 0;

      if( ltrace ) {
         printf("nestcf: dvchmcft returns %d\n",ifit);
         dvdump(fit,max_nc,"fits from dvchmcft.\n");  
      }
      if( fit[ifit] <= fit_crit ) {
         p->fit = fit[ifit];
         p->opt_nc = ifit;
         p->c = Dvcopy(cw,ifit+1);
         p = p->next;
      } else {
         p = split_FNODE(p);
      }
      free_DVEC(xw); free_DVEC(fw); free_DVEC(cw); free_DVEC(fit);
      if( ltrace ) {
         dump_FNODE_list("In loop",head);
      }
   }
   for( p = head, nseg = 0; p != NULL; p = p->next ) {
      if( p->fit < 0.0 || p->fit > fit_crit ) return 0;
      ++nseg;
   }
   if( ltrace ) {
      printf("nestcf: %d segments.\n",nseg);
   }
   for( iseg = 0, p = head; iseg < nseg; iseg++, p = p->next ) {
      c[iseg] = p->c;
      nc[iseg] = p->opt_nc;
      xseg[iseg] = x[p->i_min];
      if( iseg == nseg - 1 ) {
         xseg[nseg] = x[p->i_max];
      }
   }
   *pnseg = nseg; 
   return 1;
}


int  dvnestcf_(DVEC x, DVEC f, int *pn, DVEC c, int *pmax_nc, 
               DVEC xseg, int *pnseg) {
   int       n,       min_nc = 0,    max_nc;
   double    fit_crit = 1.0e-6; 
   int       iseg,          nseg;
   DMAT      tc;
   IVEC      nc;

   int       rc;

   int       ltrace = OFF;

   max_nc = *pmax_nc;
   n = *pn;
   nc = (int *) malloc(n * sizeof(int));
   tc = (DMAT) malloc(n * sizeof(DVEC));
   if( nestcf(x,f,n,min_nc,max_nc,fit_crit,xseg,pnseg,tc,nc) ) {
      nseg = *pnseg;
      if( ltrace ) {
         dvdump(xseg,nseg+1,"xseg");
      }
      dvls(c,0.0,(max_nc + 1) * nseg);
      for( iseg = 0; iseg < nseg; iseg++ ) {
         (void) DVcopy(tc[iseg],c + iseg * (max_nc + 1),nc[iseg] + 1);
         free_DVEC(tc[iseg]);
      }
      rc = 1;
   } else {
      fprintf(stderr,"dvnestcf_: fit failed.\n");
      rc = 0;
   }
   free(nc);
   free(tc);

   return rc;
}

/* Routine to produce "symmetric pulse" for use in producing d(phi) / dr */
/* which will integrate to 0.0 at large r ...                            */

int dvpulse_reflect(DVEC yin, DVEC rin, int nrin, DVEC yout, DVEC rout,
                    double rmin, double rmax, double roffdr, int roffp,
                    double gapdr) {

   int        ixrmin0,     ixrmax0,     ixrmin1,     ixrmax1,
              ixrmax2;
   int        i_in,        i_out,       nrout;
   double     f77_alo,     f77_bhi;
   double     rmax2;
   int        ltrace = OFF;
   
   if( (rmin - roffdr) < rin[0] || 
       (2.0 * rmax - rmin + 3.0 * roffdr + gapdr) > rin[nrin - 1] ) {
      fprintf(stderr,"dvpulse_reflect: Bounds error.\n");
      fprintf(stderr,"<%12.4e%12.4e>%12.4e%12.4e%12.4e%12.4e\n",
              rin[0],rin[nrin - 1],rmin,rmax,roffdr,gapdr);
      return 0;
   }

   f77_alo = rmin - gapdr; f77_bhi = rmax + gapdr;
   dvrollboth_(yin,yout,rin,&nrin,&f77_alo,&rmax,&roffp,&rmax,&f77_bhi,&roffp);
   ixrmin0 = ixdvnearest(rin,nrin,rmin - roffdr);
   ixrmax0 = ixdvnearest(rin,nrin,rmax + roffdr);
   ixrmin1 = ixdvnearest(rin,nrin,rmax + roffdr + gapdr);
   ixrmax1 = ixdvnearest(rin,nrin,2.0 * rmax - rmin + 3.0 * roffdr + gapdr);
   if( ltrace ) {
      printf("dvpulse_reflect: min0: %d(%g) max0 %d(%g)\n",
         ixrmin0,rin[ixrmin0],ixrmax0,rin[ixrmax0]);
      printf("dvpulse_reflect: min1: %d(%g) max1 %d(%g)\n",
         ixrmin1,rin[ixrmin1],ixrmax1,rin[ixrmax1]);
   }
   rout = DVcopy(rin,rout,ixrmin1+1);
   for( i_in = ixrmax0, i_out = ixrmin1; i_in >= ixrmin0; i_in--, i_out++ ) {
      rout[i_out] = rout[ixrmin1] + rout[ixrmax0] - rout[i_in];
      yout[i_out] = -yout[i_in];
   }
   nrout = ixrmin1 + (ixrmax0 - ixrmin0) + 1;
   ixrmax2 = ixdvnearest(rin,nrin,rout[nrout-1]);
   if( ltrace ) {
      printf("dvpulse_reflect: ixrmax2 %d(%g) rout[%d](%g)\n",
             ixrmax2,rin[ixrmax2],nrout,rout[nrout-1]);
   }
   rmax2 = rout[nrout-1];
   for( i_in = ixrmax2; i_in < nrin; i_in++ ) {
      if( rin[i_in] > rmax2 ) { 
         rout[nrout] = rin[i_in];
         yout[nrout++] = yin[i_in];
      }
   }
   return nrout;
}
