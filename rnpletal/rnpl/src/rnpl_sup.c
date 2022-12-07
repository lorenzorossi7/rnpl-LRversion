/* support files for rnpl */
/* $Header: /home/cvs/rnpl/src/rnpl_sup.c,v 1.1.1.1 2013/07/09 00:38:27 cvs Exp $ */
/* Copyright (c) 1994-1997 by Robert L. Marsa */

/* Modified by Matthew W Choptuik, Aug 1997 to use (char *) casts */
/* on strdup() call to appease 'xlc'. */

/* Modified by Matthew W Choptuik, May 2000 to fix bug in f77 initialization 
   (improper declaration of multiple grid extents, near line 9517, several
   instances of 'gl->gfunc' were replaced with 'gl->next->gfunc' as per 
   update-generating code near line 5227) */

#define _SUPPORT_
#define _SUPPORT_
#include <stdlib.h>
#include "rnpl.h"
#include "rnpl.tab.h"
#include "bbhutil.h" //LR - modern compilers seem to require this or it won't recognise function strcmp_nc
#include <math.h> // LR - moved this here from below line with "#ifdef __POWERPC__" or the compiler won't recognise it
#ifdef __POWERPC__
char * strdup(const char *);
#endif
#define MAXRANK 5
#define STR_P_SIZE 64
#define MAXCPOS 72
#define FTAB 2
#define FORSTR "\n     &      "
#define IVEC_SIZE 41

static char forbuf[256];
static int f77_coord_flag=0;
static int f77_gfunc_flag=0;

int fort_out_const(char *s, const double d)
{
  int i;
  char *p;
    
  if(fort_const_D){
    i=sprintf(s,"(%.16G",d);
    for(p=s;*p && *p!='E';p++);
    if(!*p){
      *p='D';
      *(p+1)='0';
      *(p+2)=0;
      i+=2;
    }else *p='D';    
  }else{
    i=sprintf(s,"(%.16G",d);
    for(p=s;*p && *p!='E';p++);
    if(!*p){
      *p='E';
      *(p+1)='0';
      *(p+2)=0;
      i+=2;
    }
  }
  p=s+strlen(s);
  *p=')';
  *(p+1)=0;
  return i;
}

/*  formats a string for FORTRAN.  Input string can have 
    a '\n' as the last character only.  s represents an
    indivisible chunk, that is lines are broken only before
    or after s */
    
void fort_out(FILE *fp, const char *s)
{
  static int cpos=0;
  static int scol=0;
  static int lbr=0;
  static int cr=1;
  int i;

  if(s){
    if(cr){
      for(scol=0;s[scol]==' ';scol++);
    }
    i=strlen(s);
#ifdef RNPLDEBUG
printf("fort_out(%d): got %s\n len %d\n",cpos,s,i);
#endif
    if(s[i-1]=='\n'){
      i--;
      cr=1;
    }else cr=0;
    if(cpos+i > MAXCPOS){
      if(!lbr) scol+=FTAB;
      fprintf(fp,"\n     & ");
      for(cpos=7;cpos<scol;cpos++)
        fprintf(fp," ");
      if(scol<7) scol=7;
      lbr=1;
    }
    cpos += i;
    fprintf(fp,s);
    if(cr){
      if(lbr) scol=0;
      lbr=0;
      cpos=0;
    }
  }
}

/*  formats a comment string for FORTRAN.  Input string can have 
    a '\n' as the last character only.  s represents an
    indivisible chunk, that is lines are broken only before
    or after s */
    
void fortcom_out(FILE *fp, const char *s)
{
  static int cpos=0;
  static int scol=0;
  static int lbr=0;
  static int cr=1;
  int i;

  if(s){
    if(cr){
      for(scol=0;s[scol]==' ';scol++);
    }
    i=strlen(s);
    if(s[i-1]=='\n'){
      i--;
      cr=1;
    }else cr=0;
    if(cpos+i > MAXCPOS){
      if(!lbr) scol+=FTAB;
      fprintf(fp,"\n!  ");
      for(cpos=3;cpos<scol;cpos++)
        fprintf(fp," ");
      if(scol<3) scol=3;
      lbr=1;
    }
    cpos += i;
    fprintf(fp,s);
    if(cr){
      if(lbr) scol=0;
      lbr=0;
      cpos=0;
    }
  }
}

void fatal_error(const char *msg)
{
  fprintf(stderr,"FATAL ERROR: %s\nExiting...\n",msg);
  exit(1);
}

int mystrcmp(const char *s1, const char *s2)
{
  int r;

  if(language==C || language==CPP)
    r=strcmp(s1,s2);
  else if(language==F77 || language==F90 || language==ALLF || language==UPF || language==IDF){
    r=strcmp_nc(s1,s2);
  }
  return r;
}

void dump_name(const char *s, const name_list *nm)
{
  if(nm){
    printf("%s: <%s> %d %x %x\n",s,nm->n,strlen(nm->n),nm,nm->next);
  }else{
    printf("%s: NULL\n",s);
  }
}

name_list *add_name(const char *nm)
{
  name_list *nn,*p,*r;

  if(nm){
    nn=new_name();
#ifndef _ALL_SOURCE
    nn->n=strdup(nm);
#else
    nn->n=strdup((char *)nm);
#endif
    if(nn->n==NULL){
      fprintf(stderr,"add_name: can't malloc space for %s\n",nm);
      fatal_error("No more memory");
    }
    p=&str_tbl;
    while(p->next && mystrcmp(nn->n,p->next->n)>0)
      p=p->next;
    if(p->next && !mystrcmp(nn->n,p->next->n)){
      r=p->next;
      free(nn->n);
      free(nn);
    } else {
      nn->next=p->next;
      p->next=nn;
      r=nn;
    }
  }else r=NULL;
  return r;
}

name_list *name_lookup(const char *nm)
{
  name_list *p;
  
  p=&str_tbl;
  while(p->next && mystrcmp(nm,p->next->n))
    p=p->next;
  if(p->next && !mystrcmp(nm,p->next->n))
    return p->next;
  else {
    fprintf(stderr,"Name %s does not exist in str_tbl.\n",nm);
    fatal_error("Can't find name.");
    return NULL;
  }
}

int vsize(const v_size *vs)
{
  int i,s;
  
  if(vs){
    for(s=1,i=0;i<vs->dim;i++)
      s*=vs->size[i];
  }else{
    s=0;
  }
  return s;
}

attrib_set *new_attrib_set()
{
   attrib_set *p;
   
   if((p=(attrib_set *)malloc(sizeof(attrib_set)))==NULL)
      fatal_error("Can't malloc new attrib_set.");
   p->name=NULL;
   p->next=NULL;
   return(p);
}

ivec_list *new_ivec_list()
{
  ivec_list *p;
  
  if((p=(ivec_list *)malloc(sizeof(ivec_list)))==NULL)
    fatal_error("Can't malloc new ivec_list.");
  p->next=NULL;
  return(p);
}

param_ref_list *new_param_ref()
{
  param_ref_list *pr;
  
  if((pr=(param_ref_list *)malloc(sizeof(param_ref_list)))==NULL)
    fatal_error("Can't malloc new param_ref_list.");
  pr->par=NULL;
  pr->next=NULL;
  return(pr);
}

index_list *new_index_list()
{
  index_list *in;
  
  if((in=(index_list *)malloc(sizeof(index_list)))==NULL)
    fatal_error("Can't malloc new index_list.");
  in->next=NULL;
  in->ind=NULL;
  return(in);
}

indel *new_indel()
{
  indel *in;
  
  if((in=(indel *)malloc(sizeof(indel)))==NULL)
    fatal_error("Can't malloc new indel.");
  in->val.expr=NULL;
  return(in);  
}

work_array *new_work_array()
{
  work_array *wa;
  
  if((wa=(work_array *)malloc(sizeof(work_array)))==NULL)
    fatal_error("Can't malloc new work_array.");
  wa->expr=NULL;
  return(wa);
}

work_list *new_work_list()
{
  work_list *wk;
  
  if((wk=(work_list *)malloc(sizeof(work_list)))==NULL)
    fatal_error("Can't malloc new work_list.");
  wk->next=NULL;
  wk->work=NULL;
  return(wk);
}

name_ref *new_name_ref()
{
  name_ref *nr;

  if((nr=(name_ref *)malloc(sizeof(name_ref)))==NULL)
    fatal_error("Can't malloc new name_ref.");
  nr->name=NULL;
  nr->subs=NULL;
  return (nr);
}

ref_list *new_ref_list()
{
  ref_list *rl;
  
  if((rl=(ref_list *)malloc(sizeof(ref_list)))==NULL)
    fatal_error("Can't malloc new ref_list.");
  rl->next=NULL;
  rl->data.nref=NULL;
  return(rl);
}

statement_list *new_statement_list()
{
  statement_list *s;

  if((s=(statement_list *)malloc(sizeof(statement_list)))==NULL)
    fatal_error("Can't malloc new statement_list.");
  s->next=NULL;
  s->str=NULL;
  return(s);
}

scalar_list *new_scalar_list()
{
  scalar_list *s;

  if((s=(scalar_list *)malloc(sizeof(scalar_list)))==NULL)
    fatal_error("Can't malloc new scalar_list.");
  s->next=NULL;
  return(s);
}

v_size *new_v_size()
{
  v_size *p;
  
  if((p=(v_size *)malloc(sizeof(v_size)))==NULL)
    fatal_error("Can't malloc new v_size.");
  p->size=NULL;
  return(p);
}

param_dec *new_param_dec()
{
  param_dec *p;

  if((p=(param_dec *)malloc(sizeof(param_dec)))==NULL)
    fatal_error("Can't malloc new param_dec.");
  p->name=NULL;
  p->size=NULL;
  p->next=NULL;
  return(p);
}

attrib_dec *new_attrib_dec()
{
  attrib_dec *p;

  if((p=(attrib_dec *)malloc(sizeof(attrib_dec)))==NULL)
    fatal_error("Can't malloc new attrib_dec.");
  p->name=NULL;
  p->def_val=NULL;
  p->next=NULL;
  return(p);
}

init_dec *new_init_dec()
{
  init_dec *i;

  if((i=(init_dec *)malloc(sizeof(init_dec)))==NULL)
    fatal_error("Can't malloc new init_dec.");
  i->toff=0;
  i->name=NULL;
  i->init=NULL;
  i->next=NULL;
  return(i);
}

int *new_i_ar(const int i)
{
  int *in;

  if((in=(int *)malloc(sizeof(int)*i))==NULL)
    fatal_error("Can't malloc new int array.");
  return(in);
}

double *new_f_ar(const int i)
{
  double *f;

  if((f=(double *)malloc(sizeof(double)*i))==NULL)
    fatal_error("Can't malloc new double array.");
  return(f);
}

char **new_s_ar(const int i)
{
  char **c;

  if((c=(char **)malloc(sizeof(char *)*i))==NULL)
    fatal_error("Can't malloc new char * array.");
  return(c);
}

char *new_string(const int i)
{
  char *s;

  if((s=(char *)malloc(sizeof(char)*(i+1)))==NULL)
    fatal_error("Can't malloc new string.");
  s[0]=0;
  return(s);
}

name_list *new_name()
{
  name_list *n;

  if((n=(name_list *)malloc(sizeof(name_list)))==NULL)
    fatal_error("Can't malloc new name.");
  n->next=NULL;
  n->n=NULL;
  return(n);
}

gfunction *new_gfunc()
{
  gfunction *f;

  if((f=(gfunction *)malloc(sizeof(gfunction)))==NULL)
    fatal_error("Can't malloc new gfunction.");
  f->name=NULL;
  f->indx=NULL;
  f->grd=NULL;
  return(f);
}

coord_ref *new_coord()
{
  coord_ref *c;

  if((c=(coord_ref *)malloc(sizeof(coord_ref)))==NULL)
    fatal_error("Can't malloc new coord.");
  c->name=NULL;
  c->grd=NULL;
  c->indx=NULL;
  return(c);
}

d_op *new_dop()
{
  d_op *d;

  if((d=(d_op *)malloc(sizeof(d_op)))==NULL)
    fatal_error("Can't malloc new d_op.");
  d->name=NULL;
  d->expr=NULL;
  d->clst=NULL;
  return(d);
}

node *new_node()
{
  node *n;

  if((n=(node *)malloc(sizeof(node)))==NULL)
    fatal_error("Can't malloc new node.");
  n->left=n->right=NULL;
  n->data.name=NULL;
  return(n);
}

offset_type *new_offset()
{
  offset_type *i;
  
  if((i=(offset_type *)malloc(sizeof(offset_type)))==NULL)
    fatal_error("Can't malloc new offset.");
  i->next=NULL;
  return(i);
}

func *new_func()
{
  func *f;
  
  if((f=(func *)malloc(sizeof(func)))==NULL)
    fatal_error("Can't malloc new func.");
  f->name=NULL;
  f->expr=NULL;
  return(f);
}

coord_list *new_coord_list()
{
  coord_list *c;
  
  if((c=(coord_list *)malloc(sizeof(coord_list)))==NULL)
    fatal_error("Can't malloc new coord_list.");
  c->name=NULL;
  c->next=NULL;
  return(c);
}

coord_dec *new_coord_dec()
{
  coord_dec *c;
  
  if((c=(coord_dec *)malloc(sizeof(coord_dec)))==NULL)
    fatal_error("Can't malloc new coord_dec.");
  c->c_names=NULL;
  c->name=NULL;
  c->next=NULL;
  return(c);
}

coord_table *new_coord_tab()
{
  coord_table *c;
  
  if((c=(coord_table *)malloc(sizeof(coord_table)))==NULL)
    fatal_error("Can't malloc new coord_table.");
  c->c_names=NULL;
  c->name=NULL;
  return(c);
}

update *new_update()
{
  update *u;

  if((u=(update *)malloc(sizeof(update)))==NULL)
    fatal_error("Can't malloc new update.");
  u->type=NULL;
  u->name=NULL;
  u->gfs=NULL;
  u->refs=NULL;
  u->next=NULL;
  return(u);
}

bounds *new_bounds()
{
  bounds *b;
  
  if((b=(bounds *)malloc(sizeof(bounds)))==NULL)
    fatal_error("Can't malloc new bounds.");
  b->next=NULL;
  return(b);
}

dop_ref_list *new_dop_ref()
{
  dop_ref_list *d;
  
  if((d=(dop_ref_list *)malloc(sizeof(dop_ref_list)))==NULL)
    fatal_error("Can't malloc new dop_ref_list.");
  d->dop=NULL;
  d->gf_refs=NULL;
  d->grd=NULL;
  d->next=NULL;
  return(d);
}

gfunc_ref_list *new_gfunc_ref()
{
  gfunc_ref_list *g;
  
  if((g=(gfunc_ref_list *)malloc(sizeof(gfunc_ref_list)))==NULL)
    fatal_error("Can't malloc new gfunc_ref_list.");
  g->gfunc=NULL;
  g->name=NULL;
  g->array=0;
  g->next=NULL;
  return(g);
}

gfunc_tab_list *new_gfunc_tab()
{
  gfunc_tab_list *g;
  
  if((g=(gfunc_tab_list *)malloc(sizeof(gfunc_tab_list)))==NULL)
    fatal_error("Can't malloc new gfunc_tab_list.");
  g->gfunc=NULL;
  g->next=NULL;
  return(g);
}

grid_list * new_grid_list()
{
  grid_list *g;

  if((g=(grid_list *)malloc(sizeof(grid_list)))==NULL)
    fatal_error("Can't malloc new grid_list.");
  g->cname=NULL;
  g->name=NULL;
  g->clst=NULL;
  g->ireg=NULL;
  g->creg=NULL;
  g->next=NULL;
  return(g);
}

gfunc_list * new_gfunc_list()
{
  gfunc_list *g;

  if((g=(gfunc_list *)malloc(sizeof(gfunc_list)))==NULL)
    fatal_error("Can't malloc new gfunc_list.");
  g->fname=NULL;
  g->gname=NULL;
  g->tlev=NULL;
  g->desc=NULL;
   g->atts=NULL;
  g->next=NULL;
  return(g);
}

operator * new_operator()
{
  operator *o;
  
  if((o=(operator *)malloc(sizeof(operator)))==NULL)
    fatal_error("Can't malloc new operator.");
  o->deriv=NULL;
  o->op_expr=NULL;
  o->next=NULL;
  return(o);
}

res_list * new_res_list()
{
  res_list *r;
  
  if((r=(res_list *)malloc(sizeof(res_list)))==NULL)
    fatal_error("Can't malloc new res_list.");
  r->reg=NULL;
  r->elst.expr=NULL;
  r->next=NULL;
  return(r);
}

residual * new_residual()
{
  residual *r;
  
  if((r=(residual *)malloc(sizeof(residual)))==NULL)
    fatal_error("Can't malloc new residual.");
  r->indx=NULL;
  r->toff=0;
  r->name=NULL;
  r->res=NULL;
  r->next=NULL;
  return(r);
}

i_reg * new_i_reg()
{
  i_reg *i;
  
  if((i=(i_reg *)malloc(sizeof(i_reg)))==NULL)
    fatal_error("Can't malloc new i_reg.");
  i->upper=NULL;
  i->lower=NULL;
  i->next=NULL;
  return(i);
}

c_reg * new_c_reg()
{
  c_reg *c;
  if((c=(c_reg *)malloc(sizeof(c_reg)))==NULL)
    fatal_error("Can't malloc new c_reg.");
  c->upper=NULL;
  c->lower=NULL;
  c->next=NULL;
  return(c);
}

ifstat * new_ifstat()
{
  ifstat *i;
  
  if((i=(ifstat *)malloc(sizeof(ifstat)))==NULL)
    fatal_error("Can't malloc new ifstat.");
  i->lexpr=NULL;
  i->expr=NULL;
  i->elst.expr=NULL;
  return(i);
}

void gfunc_suffix(FILE *fp, const int offset)
{
  if(offset<0)
    fprintf(fp,"_nm%d",-offset);
  else if(offset>0 && offset!=FLAG)
    fprintf(fp,"_np%d",offset);
  else if(offset==0)
    fprintf(fp,"_n");
}

void gfunc_suffix_f(FILE *fp, const int offset)
{
  forbuf[0]='\0';
  if(offset<0)
    sprintf(forbuf,"_nm%d",-offset);
  else if(offset>0 && offset!=FLAG)
    sprintf(forbuf,"_np%d",offset);
  else if(offset==0)
    sprintf(forbuf,"_n");
  fort_out(fp,forbuf);
}

void dop_to_string(FILE *fp,d_op *d)
{
  coord_list *nm;
  int i;

  fprintf(fp,"%s(",d->name->n);
  ex_to_string(fp,d->expr);
  fprintf(fp,",");
  for(i=0,nm=d->clst->c_names;i<d->clst->rank-1;i++,nm=nm->next)
    fprintf(fp,"%s,",nm->name->n);
  fprintf(fp,"%s)",nm->name->n);
}

void func_to_string(FILE *fp,func *f)
{
  if(f->name==Abs){
    fprintf(fp,"fabs((double)(");
    ex_to_string(fp,f->expr);
    fprintf(fp,"))");
  }else{
    fprintf(fp,"%s(",f->name->n);
    ex_to_string(fp,f->expr);
    fprintf(fp,")");
  }
}

void func_to_string_wk(FILE *fp,func *f)
{
  if(f->name==Abs){
    fprintf(fp,"fabs((double)(");
    ex_to_string_wk(fp,f->expr);
    fprintf(fp,"))");
  }else{
    fprintf(fp,"%s(",f->name->n);
    ex_to_string_wk(fp,f->expr);
    fprintf(fp,")");
  }
}

void func_to_string_f(FILE *fp,func *f)
{
  sprintf(forbuf,"%s(",f->name->n);
  fort_out(fp,forbuf);
  ex_to_string_f(fp,f->expr);
  fort_out(fp,")");
}

void coord_to_string(FILE *fp,coord_ref *cd)
{
  int i;
  i_reg *b;
  
  if(is_space(cd->name)){
    fprintf(fp,"%s[",cd->name->n);
    if(cd->indx->type==OFFS){
      fprintf(fp,"%c",rank(cd->name)+'i');
      if(cd->indx->val.offset < 0)
        fprintf(fp,"%d",cd->indx->val.offset);
      else if(cd->indx->val.offset>0)
        fprintf(fp,"+%d",cd->indx->val.offset);
    }else{
      fprintf(fp,"(int)(");
      ex_to_string(fp,cd->indx->val.expr);
      fprintf(fp,")");
    }
    if(cd->grd){
      for(b=cd->grd->reg.bounds,i=0;i<rank(cd->name) && b;i++,b=b->next);
      if(!b){
        fprintf(stderr,"coord_to_string: coord %s points to grid %s.\n",
                cd->name->n,cd->grd->name->n);
        fprintf(stderr,"  coord has rank %d while grid has rank %d.\n",
                rank(cd->name),cd->grd->reg.rank);
        fatal_error("Bad coordinate setup.");
      }
      if(!((b->lower->type==NUM && b->lower->data.num==0) ||
           (b->lower->type==INUM && b->lower->data.inum==0))){
        fprintf(fp,"-");
        ex_to_string(fp,b->lower);
      }
    }
    fprintf(fp,"]");
  }else{
    fprintf(fp,"%s",cd->name->n);
  }
}

void coord_to_string_f(FILE *fp,coord_ref *cd)
{
  int i;
  i_reg *b;
  
  if(is_space(cd->name)){
    if(f77_coord_flag){
      i=sprintf(forbuf,"q(%s",cd->name->n); 
    }else{
      i=sprintf(forbuf,"%s(",cd->name->n); 
    }
    if(cd->indx->type==OFFS){
      if(f77_coord_flag){
        i+=sprintf(forbuf+i,"+%c",rank(cd->name)+'i'); 
      }else{
        i+=sprintf(forbuf+i,"%c",rank(cd->name)+'i'); 
      }
      if(cd->indx->val.offset < 0)
        i+=sprintf(forbuf+i,"%d",cd->indx->val.offset);
      else if(cd->indx->val.offset>0)
        i+=sprintf(forbuf+i,"+%d",cd->indx->val.offset);
      fort_out(fp,forbuf);
    }else{
      fort_out(fp,forbuf);
      ex_to_string_f(fp,cd->indx->val.expr);
    }
    for(b=cd->grd->reg.bounds,i=0;i<rank(cd->name);i++,b=b->next);
    if(!b){
      fprintf(stderr,"coord_to_string: coord %s points to grid %s.\n",
              cd->name->n,cd->grd->name->n);
      fprintf(stderr,"  coord has rank %d while grid has rank %d.\n",
              rank(cd->name),cd->grd->reg.rank);
      fatal_error("Bad coordinate setup.");
    }
    if(f77_coord_flag){
      if(!((b->lower->type==NUM && b->lower->data.num==0) ||
           (b->lower->type==INUM && b->lower->data.inum==0))){
        fort_out(fp,"-");
        ex_to_string_f(fp,b->lower);
      }
    }
    fort_out(fp,")");
  }else{
    sprintf(forbuf,"%s",cd->name->n);
    fort_out(fp,forbuf);
  }
}

void gfunc_to_string(FILE *fp,gfunction *f)
{
  fprintf(fp," *(%s",f->name->n);
  gfunc_suffix(fp,f->toff);
  fprintf(fp," + ");
  if(f->grd && (f->grd->reg.rank>1 || (f->grd->reg.rank==1 && 
     !compare_expr(f->grd->reg.bounds->lower,f->grd->reg.bounds->upper))))
    array_ref_to_string(fp,f,f->grd);
  else fprintf(fp,"0");
  fprintf(fp,")");
}

void gfunc_to_string_f(FILE *fp,gfunction *f)
{
  if(f77_gfunc_flag){
    sprintf(forbuf,"q(ptrs(gf_st(%d)+%d)+",gfunc_to_index(name_to_gfunc(f->name))+1,
            offset_to_index(f->toff,name_to_gfunc(f->name)->tlev)+1);
    fort_out(fp,forbuf);
    if(f->grd && (f->grd->reg.rank>1 || (f->grd->reg.rank==1 && 
       !compare_expr(f->grd->reg.bounds->lower,f->grd->reg.bounds->upper))))
      array_reff_to_string(fp,f,f->grd);
    else fort_out(fp,"0");
    fort_out(fp,")");
  }else{
    sprintf(forbuf,"%s",f->name->n);
    fort_out(fp,forbuf);
    gfunc_suffix_f(fp,f->toff);
    if(f->grd && (f->grd->reg.rank>1 || (f->grd->reg.rank==1 && 
       !compare_expr(f->grd->reg.bounds->lower,f->grd->reg.bounds->upper))))
      array_ref_to_string_f(fp,f,f->grd);
    else {
      fort_out(fp,"(1)");
    }
  }
}

int get_priority(const char c)
{
  int p;
  
  switch(c){
    case '=' :  p=0;
                break;
    case 'o' :  p=1;
                break;
    case 'a' :  p=2;
                break;
    case 'e' :
    case 'n' :  p=3;
                break;
    case '>' :
    case '<' :
    case 'g' :
    case 'l' :  p=4;
                break;
    case '+' :
    case '-' :  p=5;
                break;
    case '*' :
    case '/' :  p=6;
                break;
    case '^' :  p=7;
  }
  return p;
}

void do_left_paren(FILE *fp, node *expr, char *c)
{
  int pm,pl;
  
  if(expr->type==OP && expr->left){
    pm=get_priority(expr->data.op);
    if(expr->left->type==OP){
      pl=get_priority(expr->left->data.op);
      if(pm > pl){
        fprintf(fp,c);
      }
    }
  }
}

void do_right_paren(FILE *fp, node *expr, char *c)
{
  int pm,pr;
  
  if(expr->type==OP && expr->right){
    pm=get_priority(expr->data.op);
    if(expr->right->type==OP){
      pr=get_priority(expr->right->data.op);
      if(pm > pr || ((expr->data.op=='-' || expr->data.op=='/') && pm>=pr)){
        fprintf(fp,c);
      }
    }
  }
}

void do_left_paren_f(FILE *fp, node *expr, char *c)
{
  int pm,pl;
  
  if(expr->type==OP && expr->left){
    pm=get_priority(expr->data.op);
    if(expr->left->type==OP){
      pl=get_priority(expr->left->data.op);
      if(pm > pl){
        fort_out(fp,c);
      }
    }
  }
}

void do_right_paren_f(FILE *fp, node *expr, char *c)
{
  int pm,pr;
  
  if(expr->type==OP && expr->right){
    pm=get_priority(expr->data.op);
    if(expr->right->type==OP){
      pr=get_priority(expr->right->data.op);
      if(pm > pr || ((expr->data.op=='-' || expr->data.op=='/') && pm>=pr)){
        fort_out(fp,c);
      }
    }
  }
}

/* generates code from the expression parse tree */
void ex_to_string(FILE *fp, node *expr)
{
  if(!expr)
    return;

  /* depth-first traversal of expression parse tree */
  if(expr->type==OP && expr->data.op=='^')
    fprintf(fp,"pow(");
  
  do_left_paren(fp,expr,"(");
  
  ex_to_string(fp,expr->left);

  do_left_paren(fp,expr,")");
  
  switch(expr->type){
    case INUM : fprintf(fp,"%d",expr->data.inum);
                break;
    case NUM  : fprintf(fp,"%.16g",expr->data.num);
                break;
    case OP   : switch(expr->data.op){
                  case '^' :  fprintf(fp,", ");
                              break;
                  case '%' :  fprintf(fp,"%%");
                              break;
                  case 'e' :  fprintf(fp,"==");
                              break;
                  case '<' :  fprintf(fp,"<");
                              break;
                  case '>' :  fprintf(fp,">");
                              break;
                  case 'l' :  fprintf(fp,"<=");
                              break;
                  case 'g' :  fprintf(fp,">=");
                              break;
                  case 'n' :  fprintf(fp,"!=");
                              break;
                  case 'o' :  fprintf(fp," || ");
                              break;
                  case 'a' :  fprintf(fp," && ");
                              break;
                  default  :  fprintf(fp,"%c",expr->data.op);
                              break;
                }
                break;
    case IDEN : if(is_space(expr->data.name)){
                  fprintf(fp,"%s[%c]",expr->data.name->n,'i'+rank(expr->data.name));
                }else{
                  fprintf(fp,expr->data.name->n);
                }
                break;
    case FUNC : func_to_string(fp,expr->data.func);
                break;
    case D_OP : dop_to_string(fp,expr->data.deriv);
                break;
    case GFUNC: gfunc_to_string(fp,expr->data.gfunc);
                break;
    case COORD: coord_to_string(fp,expr->data.coord);
                break;
  }

  do_right_paren(fp,expr,"(");
  
  ex_to_string(fp,expr->right);

  do_right_paren(fp,expr,")");
  
  if(expr->type==OP && expr->data.op=='^')
   fprintf(fp,")");
}

/* prints expression for auto work array size */
void ex_to_string_wk(FILE *fp, node *expr)
{
  if(!expr)
    return;

  /* depth-first traversal of expression parse tree */
  if(expr->type==OP && expr->data.op=='^')
    fprintf(fp,"pow(");
  
  do_left_paren(fp,expr,"(");
  
  ex_to_string_wk(fp,expr->left);

  do_left_paren(fp,expr,")");

  switch(expr->type){
    case INUM : fprintf(fp,"%d",expr->data.inum);
                break;
    case NUM  : fprintf(fp,"%.16g",expr->data.num);
                break;
    case OP   : switch(expr->data.op){
                  case '^' :  fprintf(fp,", ");
                              break;
                  case '%' :  fprintf(fp,"%%");
                              break;
                  case 'e' :  fprintf(fp,"==");
                              break;
                  case '<' :  fprintf(fp,"<");
                              break;
                  case '>' :  fprintf(fp,">");
                              break;
                  case 'l' :  fprintf(fp,"<=");
                              break;
                  case 'g' :  fprintf(fp,">=");
                              break;
                  case 'n' :  fprintf(fp,"!=");
                              break;
                  case 'o' :  fprintf(fp," || ");
                              break;
                  case 'a' :  fprintf(fp," && ");
                              break;
                  default  :  fprintf(fp,"%c",expr->data.op);
                              break;
                }
                break;
    case IDEN : 
                      if(is_space(expr->data.name)){
                  fprintf(fp,"%s[%c]",expr->data.name->n,'i'+rank(expr->data.name));
                }else if(is_gbase(expr->data.name)){
                   int i;
                   i=gbase_to_gindex(expr->data.name);
                   if(i==-1){
                      fprintf(stderr,"ERROR: ex_to_string_wk: grid base %s doesn't match with any grids\n",
                              expr->data.name->n);
                      fatal_error("Bad work array size expression.");
                   }else{
                      fprintf(fp,"%s_%s",grids[i].name->n,expr->data.name->n);
                   }
                }else{
                  fprintf(fp,expr->data.name->n);
                }
                break;
    case FUNC : func_to_string_wk(fp,expr->data.func);
                break;
  }

  do_right_paren(fp,expr,"(");
  
  ex_to_string_wk(fp,expr->right);

  do_right_paren(fp,expr,")");
  
  if(expr->type==OP && expr->data.op=='^')
   fprintf(fp,")");
}

void ex_to_string_f(FILE *fp, node *expr)
{
  if(!expr)
    return;

  /* depth-first traversal of expression parse tree */
  if(expr->type==OP && expr->data.op=='%')
    fort_out(fp,"mod(");
  
  do_left_paren_f(fp,expr,"(");
  
  ex_to_string_f(fp,expr->left);

  do_left_paren_f(fp,expr,")");
  
  switch(expr->type){
    case INUM : sprintf(forbuf,"%d",expr->data.inum);
                fort_out(fp,forbuf);
                break;
    case NUM  : fort_out_const(forbuf,expr->data.num);
                fort_out(fp,forbuf);
                break;
    case OP   : switch(expr->data.op){
                  case '^' :  fort_out(fp,"**");
                              break;
                  case '%' :  fort_out(fp,",");
                              break;
                  case 'e' :  fort_out(fp,".eq.");
                              break;
                  case '<' :  fort_out(fp,".lt.");
                              break;
                  case '>' :  fort_out(fp,".gt.");
                              break;
                  case 'l' :  fort_out(fp,".le.");
                              break;
                  case 'g' :  fort_out(fp,".ge.");
                              break;
                  case 'n' :  fort_out(fp,".ne.");
                              break;
                  case 'o' :  fort_out(fp,".or.");
                              break;
                  case 'a' :  fort_out(fp,".and.");
                              break;
                  default  :  sprintf(forbuf,"%c",expr->data.op);
                              fort_out(fp,forbuf);
                              break;
                }
                break;
    case IDEN : if(is_space(expr->data.name)){
                  sprintf(forbuf,"q(getpcoord(1,%d)+%c)",
                          rank(expr->data.name),'i'+rank(expr->data.name));
                  fort_out(fp,forbuf);
                }else{
                           fort_out(fp,expr->data.name->n);
                }
                break;
    case FUNC : func_to_string_f(fp,expr->data.func);
                break;
    case D_OP : dop_to_string(fp,expr->data.deriv);
                break;
    case GFUNC: gfunc_to_string_f(fp,expr->data.gfunc);
                break;
    case COORD: coord_to_string_f(fp,expr->data.coord);
                break;
  }

  do_right_paren_f(fp,expr,"(");
  
  ex_to_string_f(fp,expr->right);

  do_right_paren_f(fp,expr,")");
  
  if(expr->type==OP && expr->data.op=='%')
    fort_out(fp,")");
}

/* generates code from the expression parse tree */
void ireg_to_string(FILE *fp, node *expr, const name_list *nm)
{
  if(!expr)
    return;

  /* depth-first traversal of expression parse tree */
  if(expr->type==OP && expr->data.op=='^')
    fprintf(fp,"pow(");
  
  do_left_paren(fp,expr,"(");
  
  ireg_to_string(fp,expr->left,nm);

  do_left_paren(fp,expr,")");
  
  switch(expr->type){
    case INUM : fprintf(fp,"%d",expr->data.inum);
                break;
    case NUM  : fprintf(fp,"%.16g",expr->data.num);
                break;
    case OP   : switch(expr->data.op){
                  case '^' :  fprintf(fp,", ");
                              break;
                  case '%' :  fprintf(fp,"%%");
                              break;
                  case 'e' :  fprintf(fp,"==");
                              break;
                  case '<' :  fprintf(fp,"<");
                              break;
                  case '>' :  fprintf(fp,">");
                              break;
                  case 'l' :  fprintf(fp,"<=");
                              break;
                  case 'g' :  fprintf(fp,">=");
                              break;
                  case 'n' :  fprintf(fp,"!=");
                              break;
                  case 'o' :  fprintf(fp," || ");
                              break;
                  case 'a' :  fprintf(fp," && ");
                              break;
                  default  :  fprintf(fp,"%c",expr->data.op);
                              break;
                }
                break;
    case IDEN : if(is_space(expr->data.name)){
                  fprintf(fp,"%s[%c]",expr->data.name->n,'i'+rank(expr->data.name));
                }else if(is_gbase(expr->data.name)){
                  fprintf(fp,"%s_%s",nm->n,expr->data.name->n);
                }else{
                  fprintf(fp,expr->data.name->n);
                }
                break;
  }

  do_right_paren(fp,expr,"(");
  
  ireg_to_string(fp,expr->right,nm);

  do_right_paren(fp,expr,")");
  
  if(expr->type==OP && expr->data.op=='^')
    fprintf(fp,")");
}

void ireg_to_string_f(FILE *fp, node *expr, const name_list *nm)
{
  if(!expr)
    return;

  /* depth-first traversal of expression parse tree */
  if(expr->type==OP && expr->data.op=='%')
    fort_out(fp,"mod(");
  
  do_left_paren_f(fp,expr,"(");
  
  ireg_to_string_f(fp,expr->left,nm);

  do_left_paren_f(fp,expr,")");
  
  switch(expr->type){
    case INUM : sprintf(forbuf,"%d",expr->data.inum);
                fort_out(fp,forbuf);
                break;
    case NUM  : fort_out_const(forbuf,expr->data.num);
                fort_out(fp,forbuf);
                break;
    case OP   : switch(expr->data.op){
                  case '^' :  fort_out(fp,"**");
                              break;
                  case '%' :  fort_out(fp,",");
                              break;
                  case 'e' :  fort_out(fp,".eq.");
                              break;
                  case '<' :  fort_out(fp,".lt.");
                              break;
                  case '>' :  fort_out(fp,".gt.");
                              break;
                  case 'l' :  fort_out(fp,".le.");
                              break;
                  case 'g' :  fort_out(fp,".ge.");
                              break;
                  case 'n' :  fort_out(fp,".ne.");
                              break;
                  case 'o' :  fort_out(fp,".or.");
                              break;
                  case 'a' :  fort_out(fp,".and.");
                              break;
                  default  :  sprintf(forbuf,"%c",expr->data.op);
                              fort_out(fp,forbuf);
                              break;
                }
                break;
    case IDEN : if(is_space(expr->data.name)){
                  sprintf(forbuf,"q(getpcoord(1,%d)+%c)",
                          rank(expr->data.name),'i'+rank(expr->data.name));
                  fort_out(fp,forbuf);
                }else if(is_gbase(expr->data.name)){
                  sprintf(forbuf,"%s_%s",nm->n,expr->data.name->n);
                  fort_out(fp,forbuf);
                }else{
                  fort_out(fp,expr->data.name->n);
                }
                break;
  }

  do_right_paren_f(fp,expr,"(");
  
  ireg_to_string_f(fp,expr->right,nm);

  do_right_paren_f(fp,expr,")");
  
  if(expr->type==OP && expr->data.op=='%')
    fort_out(fp,")");
}

/*  language==F77
    outputs array bounds for declaration
*/
void array_bounds_to_string(FILE *fp, const grid_table *gr, const int ar)
{
  int i;
  i_reg *ir;
  
  fort_out(fp,"(");
  for(i=0,ir=gr->reg.bounds;i<gr->reg.rank-1;i++,ir=ir->next){
    ireg_to_string_f(fp,ir->lower,gr->name);
    fort_out(fp,":");
    ireg_to_string_f(fp,ir->upper,gr->name);
    fort_out(fp,",");
  }
  ireg_to_string_f(fp,ir->lower,gr->name);
  fort_out(fp,":");
  ireg_to_string_f(fp,ir->upper,gr->name);
  if(!ar)
    fort_out(fp,")");
}

void array_reff0_to_string(FILE *fp, const grid_table *gr)
{
  int i,j;
  char c;
  i_reg *b;
  
  if(gr){
    for(c='i',i=0,b=gr->reg.bounds;i<gr->reg.rank;i++,c++,b=b->next){
      if(i>0){
        sprintf(forbuf,"+(%c",c);
        fort_out(fp,forbuf);
      }else{
        sprintf(forbuf,"(%c",c);
        fort_out(fp,forbuf);
      }
      if(!((b->lower->type==NUM && b->lower->data.num==0) || 
           (b->lower->type==INUM && b->lower->data.inum==0))){
        fort_out(fp,"-");
        ex_to_string_f(fp,b->lower);
      }
      fort_out(fp,")");
      for(j=0;j<i;j++){
        sprintf(forbuf,"*getshape(%d,%d)",gptr_to_index(gr)+1,j+1);
        fort_out(fp,forbuf);
      }
    }
  }
}

void array_reff_to_string(FILE *fp, const gfunction *f, const grid_table *gr)
{
  int i,j;
  char c;
  i_reg *b;
  index_list *il;
  
  if(gr){
    for(c='i',i=0,b=gr->reg.bounds,il=f->indx;i<gr->reg.rank;i++,c++,b=b->next,il=il->next){
      if(il->ind->type==OFFS){
        if(i>0){
          sprintf(forbuf,"+(%c",c);
          fort_out(fp,forbuf);
        }else{
          sprintf(forbuf,"(%c",c);
          fort_out(fp,forbuf);
        }
        if(il->ind->val.offset < 0){
          sprintf(forbuf,"%d",il->ind->val.offset);
          fort_out(fp,forbuf);
        }else if(il->ind->val.offset > 0){
          sprintf(forbuf,"+%d",il->ind->val.offset);
          fort_out(fp,forbuf);
        }
      }else{
        if(i>0){
          fort_out(fp,"+(");
        }else{
          fort_out(fp,"(");
        }
        ex_to_string_f(fp,il->ind->val.expr);
      }
      if(!((b->lower->type==NUM && b->lower->data.num==0) || 
           (b->lower->type==INUM && b->lower->data.inum==0))){
        fort_out(fp,"-");
        ex_to_string_f(fp,b->lower);
      }
      fort_out(fp,")");
      for(j=0;j<i;j++){
        sprintf(forbuf,"*getshape(%d,%d)",gptr_to_index(gr)+1,j+1);
        fort_out(fp,forbuf);
      }
    }
  }
}

//LR moved this here from below - it seems that we need to define function coordsys_to_index before calling it the first time in modern compilers//
int coordsys_to_index(const coord_table *cd)
{
   int i;
   
   if(cd >= coords)
      i=(int)(cd-coords);
   else fatal_error("coordsys_to_index: pointer out of range.");
   return i;
}
///////////////////////////////////////////////////////

void array_ref0_to_string(FILE *fp, const grid_table *gr)
{
  int i,j;
  char c;
  i_reg *b;
  
  if(gr){
    for(c='i',i=0,b=gr->reg.bounds;i<gr->reg.rank;i++,c++,b=b->next){
      if(i>0)
        fprintf(fp,"+(%c",c);
      else fprintf(fp,"(%c",c);
      if(!((b->lower->type==NUM && b->lower->data.num==0) || 
           (b->lower->type==INUM && b->lower->data.inum==0))){
        fprintf(fp,"-");
/*        ex_to_string(fp,b->lower);*/
            ireg_to_string(fp,b->lower,gr->name);
      }
      fprintf(fp,")");
      for(j=i+1;j<gr->reg.rank;j++)
        fprintf(fp,"*%s_%s",gr->name->n,grid_base[coordsys_to_index(gr->crds)][j]->n);
/*        fprintf(fp,"*%s_%d.n_coord",gr->name->n,j);*/
    }
  }
}

void array_ref0_to_string_f(FILE *fp, const grid_table *gr)
{
  int i,j;
  char c;
  i_reg *b;
  
  if(gr){
    fort_out(fp,"(");
    for(c='i',i=0;i<gr->reg.rank;i++,c++){
      sprintf(forbuf,"%c",c);
      fort_out(fp,forbuf);
      if(i+1<gr->reg.rank)
        fort_out(fp,",");
    }
    fort_out(fp,")");
  }
}

/* prints the array reference code to fp */
void array_ref_to_string(FILE *fp, const gfunction *f, const grid_table *gr)
{
  int i,j;
  char c;
  i_reg *b;
  offset_type *o;
  index_list *il;
  
  if(gr){
    for(c='i',i=0,b=gr->reg.bounds,il=f->indx;i<gr->reg.rank;
        i++,c++,b=b->next,il=il->next){
      if(il->ind->type==OFFS){
        if(i>0)
          fprintf(fp,"+(%c",c);
        else fprintf(fp,"(%c",c);
        if(il->ind->val.offset < 0)
          fprintf(fp,"%d",il->ind->val.offset);
        else if(il->ind->val.offset > 0)
          fprintf(fp,"+%d",il->ind->val.offset);
      }else{
        if(i>0)
          fprintf(fp,"+((int)(");
        else fprintf(fp,"((int)(");
/*        ex_to_string(fp,il->ind->val.expr);*/
            ireg_to_string(fp,il->ind->val.expr,gr->name);
        fprintf(fp,")");
      }
      if(!((b->lower->type==NUM && b->lower->data.num==0) || 
           (b->lower->type==INUM && b->lower->data.inum==0))){
        fprintf(fp,"-");
/*        ex_to_string(fp,b->lower);*/
            ireg_to_string(fp,b->lower,gr->name);
      }
      fprintf(fp,")");
      for(j=i+1;j<gr->reg.rank;j++)
        fprintf(fp,"*%s_%s",gr->name->n,grid_base[coordsys_to_index(gr->crds)][j]->n);
/*        fprintf(fp,"*%s_%d.n_coord",gr->name->n,j);*/
    }
  }
}

void array_ref_to_string_f(FILE *fp, const gfunction *f, const grid_table *gr)
{
  int i,j;
  char c;
  i_reg *b;
  offset_type *o;
  index_list *il;
  
  if(gr){
    fort_out(fp,"(");
    for(c='i',i=0,b=gr->reg.bounds,il=f->indx;i<gr->reg.rank;
        i++,c++,b=b->next,il=il->next){
      if(il->ind->type==OFFS){
        j=sprintf(forbuf,"%c",c);
        if(il->ind->val.offset < 0)
          j+=sprintf(forbuf+j,"%d",il->ind->val.offset);
        else if(il->ind->val.offset > 0)
          j+=sprintf(forbuf+j,"+%d",il->ind->val.offset);
        fort_out(fp,forbuf);
      }else{
/*        ex_to_string_f(fp,il->ind->val.expr);*/
            ireg_to_string_f(fp,il->ind->val.expr,gr->name);
      }
      if(i+1<gr->reg.rank)
        fort_out(fp,",");
    }
    fort_out(fp,")");
  }
}

void grid_size(FILE *fp,const grid_table *gr)
{
  if(gr){
    if(language!=ALLF){
        coord_list *cl;
  
      cl=gr->clst;
      fprintf(fp,"%s_N%s",gr->name->n,cl->name->n);
      cl=cl->next;
      while(cl!=NULL){
        fprintf(fp,"*%s_N%s",gr->name->n,cl->name->n);
        cl=cl->next;
      }
    }else{
      sprintf(forbuf,"getsize(%d)",gptr_to_index(gr)+1);
      fort_out(fp,forbuf);
    }
  }
}

void grid_size_r(FILE *fp,const grid_table *gr)
{
  if(gr){
    if(language!=ALLF){
        coord_list *cl;
  
      cl=gr->clst;
      fprintf(fp,"N%s",cl->name->n);
      cl=cl->next;
      while(cl!=NULL){
        fprintf(fp,"*N%s",cl->name->n);
        cl=cl->next;
      }
    }
  }
}

void show_name_type(const name_list *nm)
{
  fprintf(stderr,"%s is a",nm->n);
  switch(name_type(nm)){
    case COORDINATES  :  fprintf(stderr," coordinate system.\n");
                        break;
    case COORD        :  fprintf(stderr," coordinate.\n");
                        break;
    case GRID          :  fprintf(stderr," grid.\n");
                        break;
    case GFUNC        :  fprintf(stderr," grid function.\n");
                        break;
    case PARAM        :  fprintf(stderr," parameter.\n");
                        break;
    case CONST        : fprintf(stderr," constant.\n");
                        break;
    case ATTRIB        :  fprintf(stderr,"n attribute.\n");
                        break;
    case D_OP          :  fprintf(stderr," derivative operator.\n");
                        break;
    case FUNC          :  fprintf(stderr," function.\n");
                        break;
    case UPDATE        :  fprintf(stderr,"n update.\n");
                        break;
    case UPFILE        :  fprintf(stderr,"n update file.\n");
                        break;
    case LOOPER        :  fprintf(stderr," loop driver.\n");
                        break;
    case CDIF          :  fprintf(stderr," coordinate differential.\n");
                        break;
    case GBASE        :  fprintf(stderr," grid base.\n");
                        break;
    default            : fprintf(stderr,"n unknown type.\n");
                        break;
  }
}

/*  returns an integer for the type of the object named nm
    COORDINATES  - coordinate system
    COORD        - coordinate
    GRID        - grid
    GFUNC        - grid function
    PARAM        - parameter ( any non const parameter )
    CONST        - constant
    ATTRIB      - attribute
    D_OP        - derivative operator
    FUNC        - function name
    UPDATE      - update name
    UPFILE      - update file name
    LOOPER      - looper name
    CDIF        - coordinate differential
    GBASE        -  grid base
    -1          - invalid object */
int name_type(const name_list *nm)
{
  int tp;
  
  if(is_coord(nm))
    tp=COORD;
  else if(name_to_gfunc(nm))
    tp=GFUNC;
  else if(is_param(nm) && !is_const(nm))
    tp=PARAM;
  else if(is_const(nm))
    tp=CONST;
  else if(is_coord_sys(nm))
    tp=COORDINATES;
  else if(is_grid(nm))
    tp=GRID;
  else if(is_attrib(nm))
    tp=ATTRIB;
  else if(is_dop(nm))
    tp=D_OP;
  else if(is_func(nm))
    tp=FUNC;
  else if(is_update(nm))
    tp=UPDATE;
  else if(is_upfile(nm))
    tp=UPFILE;
  else if(nm==loop_driver)
    tp=LOOPER;
  else if(is_cdif(nm))
    tp=CDIF;
  else if(is_gbase(nm))
    tp=GBASE;
  else if(nm==name_lookup("auto"))
    tp=UPFILE;
  else if(nm==name_lookup("stub"))
    tp=UPFILE;
  else tp=-1;
  return tp;
}

/* returns 1 if nm is the name of a grid base, 0 otherwise */
int is_gbase(const name_list *nm)
{
  int i,j,t=0;

  for(i=0;i<ncoords;i++)
    for(j=0;j<coords[i].rank-1 && !t;j++)
      if(nm==grid_base[i][j])
        t=1;
  return t;
}

/* returns 1 if nm is the name of a coordinate differential, 0 otherwise */
int is_cdif(const name_list *nm)
{
  int i,j,t=0;

  for(i=0;i<ncoords;i++)
    for(j=0;j<coords[i].rank && !t;j++)
      if(nm==coord_difs[i][j])
        t=1;
  return t;
}

/* returns 1 if nm is the name of an update file, 0 otherwise */
int is_upfile(const name_list *nm)
{
  int i;
  
  for(i=0;i<nupdates && nm!=updates[i].type;i++);
  if(i==nupdates)
    return 0;
  else return 1;
}

/* returns 1 if nm is the name of an update, 0 otherwise */
int is_update(const name_list *nm)
{
  int i;
  
  for(i=0;i<nupdates && nm!=updates[i].name;i++);
  if(i==nupdates)
    return 0;
  else return 1;
}

/* returns 1 if nm is the name of a fuction, 0 otherwise */
int is_func(const name_list *nm)
{
  if(nm==Sin || nm==Cos || nm==Tan || nm==Exp || nm==Ln || nm==Sqrt || 
     nm==Sinh || nm==Cosh || nm==Tanh || nm==ASin || nm==ACos || nm==ATan || nm==Abs)
    return 1;
  else return 0;  
}

/* returns 1 if nm is the name of a derivative operator, 0 otherwise */
int is_dop(const name_list *nm)
{
  int i;
  
  for(i=0;i<nopers && nm!=dopers[i].name;i++);
  if(i==nopers)
    return 0;
  else return 1;
}

/* returns 1 if nm is the name of an attribute, 0 otherwise */
int is_attrib(const name_list *nm)
{
  int i;
  
  for(i=0;i<nattribs && nm!=attribs[i].name;i++);
  if(i==nattribs)
    return 0;
  else return 1;
}

/* returns 1 if nm is the name of a coordinate system, 0 otherwise */
int is_coord_sys(const name_list *nm)
{
  int i;
  
  for(i=0;i<ncoords && nm!=coords[i].name;i++);
  if(i==ncoords)
    return 0;
  else return 1;
}

/* returns 1 if nm is the name of a grid, 0 otherwise */
int is_grid(const name_list *nm)
{
  int i;
  
  for(i=0;i<ngrids && nm!=grids[i].name;i++);
  if(i==ngrids)
    return 0;
  else return 1;
}

/* returns nonzero if nm is a parameter, zero otherwise */
int is_param(const name_list *nm)
{
  int i;
  
  for(i=0;i<nparams && nm!=params[i].name;i++);
  if(i==nparams)
    return 0;
  else return 1;
}

/* returns nonzero if nm is a const parameter, zero otherwise */
int is_const(const name_list *nm)
{
  int i;
  
  for(i=0;i<nparams && nm!=params[i].name;i++);
  if(i==nparams)
    return 0;
  else return params[i].con;
}

/* returns a pointer to the attribute named nm */
attrib_table *name_to_attrib(const name_list *nm)
{
  int i;
  
  for(i=0;i<nattribs && attribs[i].name != nm;i++);
  if(i==nattribs)
    return NULL;
  else return &attribs[i];
}

/* returns a pointer to the gfunc named nm */
gfunc_table *name_to_gfunc(const name_list *nm)
{
  int i;
  
  for(i=0;i<ngfuncs && gfuncs[i].fname != nm;i++);
  if(i==ngfuncs)
    return NULL;
  else return &gfuncs[i];
}

/* returns a pointer to the d_op named nm with respect to coordinates cl */
dop_table *find_dop(const name_list *nm, const coord_table *cl)
{
  int i;
  
  for(i=0;i<nopers && (nm!=dopers[i].name || !compare_ctab(cl,dopers[i].clst));i++);
  if(i==nopers)
    return NULL;
  else return &dopers[i];
}

/* returns index of first resid for gfunc nm or -1 */
int resid_exists(const name_list *nm)
{
  int i;

  for(i=0;i<nresids && resids[i].gfunc->fname!=nm;i++);
  if(i==nresids)
    return -1;
  else return i;
}

/* returns index of first init for gfunc nm or -1 */
int init_exists(const name_list *nm)
{
  int i;

  for(i=0;i<ninits && inits[i].gfunc->fname!=nm;i++);
  if(i==ninits)
    return -1;
  else return i;
}

/* returns index of first init for gfunc nm and offset toff or -1 */
int initoff_exists(const name_list *nm, const int toff)
{
  int i;

  for(i=0;i<ninits && (inits[i].gfunc->fname!=nm || inits[i].toff!=toff);i++);
  if(i==ninits)
    return -1;
  else return i;
}

/* returns index of non-auto initializer or -1 */
int initer_exists(const name_list *nm)
{
   int i,done=0;
  gfunc_tab_list *gfs;
   
   for(i=0;i<niniters && !done;i++){
      if(initers[i].header){
         for(gfs=initers[i].gfs;gfs!=NULL;gfs=gfs->next){
            if(gfs->gfunc->fname==nm){
               done=1;
            }
         }
      }
   }
   if(i==niniters)
      return -1;
   else return i;
}

/* returns TRUE if nm appears anywhere in c1->c_names */
int in_clst(const name_list *nm, const coord_table *c1)
{
  int i,res=0;
  coord_list *n1;
  
  for(i=0,n1=c1->c_names;i<c1->rank && !res;i++,n1=n1->next)
    if(nm==n1->name)
      res=1;
  return res;
}

/* returns nonzero if nm is a coordinate, zero otherwise */
int is_coord(const name_list *nm)
{
  int i,t=0;
  for(i=0;i<ncoords;i++)
    t+=in_clst(nm,&coords[i]);
  return t;
}

int is_space(const name_list *nm)
{
  int i,t=0;
  for(i=0;i<ncoords;i++)
    t+=(in_clst(nm,&coords[i]) && nm!=coords[i].c_names->name);
  return t;
}

int rank(const name_list *nm)
{
  int i,j,k=-1;
  coord_list *n;
  
  for(j=0;j<ncoords && k<0;j++)
   for(i=1,n=coords[j].c_names->next;i<coords[j].rank && k<0;i++,n=n->next)
     if(nm==n->name)
       k=i-1;
  return k;
}

int compare_ireg(const i_reg *i1, const i_reg *i2)
{
  const i_reg *p,*q;
  int res=1;
  
  for(p=i1,q=i2;p!=NULL && q!=NULL && res;p=p->next,q=q->next){
    res*=compare_expr(p->lower,q->lower);
    res*=compare_expr(p->upper,q->upper);
    res*=(p->inc==q->inc);
  }
  if(res && (p!=NULL || q!=NULL))
    res=0;
  return res;
}

/* true if c1 and c2 contain the same elements regardless of order */
int compare_ctab(const coord_table *c1, const coord_table *c2)
{
  int res=1,i,j;
  coord_list *n1,*n2;
  
  if(c1->rank != c2->rank)
    res=0;
  else {
    char *u1,*u2;

    u1=new_string(c1->rank-1);
    u2=new_string(c1->rank-1);
    for(i=0;i<c1->rank;i++){
      u1[i]=u2[i]=0;
    }
    for(n1=c1->c_names,i=0;i<c1->rank;i++,n1=n1->next){
      for(n2=c2->c_names,j=0;j<c1->rank && (n1->name!=n2->name || u2[j]);j++,n2=n2->next);
      if(j<c1->rank){
        u1[i]=u2[j]=1;
      }
    }
    for(i=0;i<c1->rank;i++)
      res*=u1[i];
  }
  return res;   
}

int compare_offset(const offset_type *i1, const offset_type *i2)
{
  int res=1;
  const offset_type *j1,*j2;
  
  if(!i1 && !i2)
    res=1;
  else if(!i1 || !i2)
    res=0;
  else {
    for(j1=i1,j2=i2;j1 && j2 && res;j1=j1->next,j2=j2->next)
      res*=(j1->offset==j2->offset);
    if(j1 || j2)
      res=0;
  }
  return res;
}

int compare_index(const index_list *i1, const index_list *i2)
{
  int res=1;
  const index_list *j1,*j2;
  
  if(!i1 && !i2)
    res=1;
  else if(!i1 || !i2)
    res=0;
  else {
    for(j1=i1,j2=i2;j1 && j2 && res;j1=j1->next,j2=j2->next){
      res*=(j1->ind->type==j2->ind->type);
      if(res)
        if(j1->ind->type==OFFS)
          res*=(j1->ind->val.offset==j2->ind->val.offset);
        else res*=compare_expr(j1->ind->val.expr,j2->ind->val.expr);
    }
    if(j1 || j2)
      res=0;
  }
  return res;
}

int compare_expr(const node *e1, const node *e2)
{
  int res;
  
  if(!e1 && !e2)
    res=1;
  else if(!e1 || !e2)
    res=0;
  else if(e1->type != e2->type)
    if(e1->type==INUM && e2->type==NUM)
      res=(e1->data.inum == e2->data.num);
    else if(e1->type==NUM && e2->type==INUM)
      res=(e1->data.num == e2->data.inum);
    else res=0;
  else {
    switch(e1->type){
      case INUM : if(e1->data.inum!=e2->data.inum)
                    res=0;
                  else {
                    res=compare_expr(e1->left,e2->left);
                    if(res)
                      res=compare_expr(e1->right,e2->right);
                  }
                  break;
      case NUM  : if(e1->data.num!=e2->data.num)
                    res=0;
                  else {
                    res=compare_expr(e1->left,e2->left);
                    if(res)
                      res=compare_expr(e1->right,e2->right);
                  }
                  break;
      case OP   : if(e1->data.op != e2->data.op)
                    res=0;
                  else {
                    res=compare_expr(e1->left,e2->left);
                    if(res)
                      res=compare_expr(e1->right,e2->right);
                  }
                  break;
      case IDEN : if(e1->data.name!=e2->data.name)
                    res=0;
                  else {
                    res=compare_expr(e1->left,e2->left);
                    if(res)
                      res=compare_expr(e1->right,e2->right);
                  }
                  break;
      case D_OP : if(e1->data.deriv->name!=e2->data.deriv->name)
                    res=0;
                  else if(!compare_ctab(e1->data.deriv->clst,e2->data.deriv->clst))
                    res=0;
                  else res=compare_expr(e1->data.deriv->expr,e2->data.deriv->expr);
                  break;
      case FUNC : if(e1->data.func->name!=e2->data.func->name)
                    res=0;
                  else res=compare_expr(e1->data.func->expr,e2->data.func->expr);
                  break;
      case COORD: if(e1->data.coord->name!=e2->data.coord->name)
                    res=0;
                  else if(e1->data.coord->grd != e2->data.coord->grd)
                    res=0;
                  else if(e1->data.coord->indx->type != e2->data.coord->indx->type)
                    res=0;
                  else if(e1->data.coord->indx->type==OFFS)
                    res=(e1->data.coord->indx->val.offset==e2->data.coord->indx->val.offset);
                  else res=compare_expr(e1->data.coord->indx->val.expr,
                                        e2->data.coord->indx->val.expr);
                  ;
                  break;
      case GFUNC: if(e1->data.gfunc->toff != e2->data.gfunc->toff)
                    res=0;
                  else if(e1->data.gfunc->name!=e2->data.gfunc->name)
                    res=0;
                  else if(!compare_index(e1->data.gfunc->indx,e2->data.gfunc->indx))
                    res=0;
                  else {
                    res=compare_expr(e1->left,e2->left);
                    if(res)
                      res=compare_expr(e1->right,e2->right);
                  }
                  break;
    }
  }
  return res;      
}

void del_clst(coord_list *cl)
{
  coord_list *p,*q;
  
  for(p=cl;p!=NULL;p=q){

#ifdef RNPLDEBUG
fprintf(stderr,"p->name->n=%s\n",p->name->n);
#endif

    q=p->next;
    free(p);
  }
}

coord_list *dup_clst(coord_list *cl)
{
  coord_list *n,*c1;
    
  if(cl){
    n=new_coord_list();
    for(c1=n;cl!=NULL;c1=c1->next,cl=cl->next){
      c1->name=cl->name;
      if(cl->next)
        c1->next=new_coord_list();
    }
  }else n=NULL; 
  return(n);
}

coord_table *dup_ctab(coord_table *cl)
{
  coord_table *n;
    
  if(cl){
    n=new_coord_tab();
    n->rank=cl->rank;
    n->c_names=dup_clst(cl->c_names);
    n->name=cl->name;
  }else n=NULL; 
  return(n);
}

gfunc_ref_list *dup_gf_ref(gfunc_ref_list *grl)
{
  if(grl){
    gfunc_ref_list *n,*g1,*g2;
    
    n=new_gfunc_ref();
    for(g1=n,g2=grl;g2!=NULL;g1=g1->next,g2=g2->next){
      g1->gfunc=g2->gfunc;
      g1->toff=g2->toff;
      g1->array=g2->array;
      g1->name=g2->name;
      if(g2->next)
        g1->next=new_gfunc_ref();
      else g1->next=NULL;
    }
    return(n);
  }else return(NULL); 
}

param_ref_list *dup_param_ref(param_ref_list *prl)
{
  if(prl){
    param_ref_list *n,*p1,*p2;
    
    n=new_param_ref();
    for(p1=n,p2=prl;p2!=NULL;p1=p1->next,p2=p2->next){
      p1->par=p2->par;
      if(p2->next)
        p1->next=new_param_ref();
      else p1->next=NULL;
    }
    return(n);
  }else return(NULL); 
}

work_list *dup_work_list(work_list *wl)
{
   if(wl){
      work_list *n,*w1,*w2;
      work_array *wk;
      
      n=new_work_list();
      for(w1=n,w2=wl;w2!=NULL;w1=w1->next,w2=w2->next){
        wk=new_work_array();
      wk->ato=w2->work->ato;
      wk->num=w2->work->num;
      wk->expr=dup_expr(w2->work->expr);
      w1->work=wk;
         if(w2->next)
            w1->next=new_work_list();
         else w1->next=NULL;
      }
      return(n);
   }else return(NULL);
}

indel *dup_indel(indel *indx)
{
  if(indx){
    indel *n;
    
    n=new_indel();
    n->type=indx->type;
    if(n->type==OFFS)
     n->val.offset=indx->val.offset;
    else n->val.expr=dup_expr(indx->val.expr);
    return(n);
  }else return(NULL);
}

index_list *dup_indx(index_list *indx)
{
  if(indx){
    index_list *o,*o1,*o2;
    
    o=new_index_list();
    for(o1=o,o2=indx;o2!=NULL;o1=o1->next,o2=o2->next){
      o1->ind=dup_indel(o2->ind);
      if(o2->next)
        o1->next=new_index_list();
      else o1->next=NULL;
    }
    return(o);
  }else return(NULL); 
}

node *dup_expr(node *expr)
{
  if(expr){    
    node *n;

    n=new_node();
    n->type=expr->type;
    switch(n->type){
      case OP   : n->data.op=expr->data.op;
                  break;
      case IDEN : n->data.name=expr->data.name;
                  break;
      case INUM : n->data.inum=expr->data.inum;
                  break;
      case NUM  : n->data.num=expr->data.num;
                  break;
      case D_OP : n->data.deriv=new_dop();
                  n->data.deriv->expand=expr->data.deriv->expand;
                  n->data.deriv->name=expr->data.deriv->name;
                  n->data.deriv->clst=dup_ctab(expr->data.deriv->clst);
                  n->data.deriv->expr=dup_expr(expr->data.deriv->expr);
                  break;
      case FUNC : n->data.func=new_func();
                  n->data.func->name=expr->data.func->name;
                  n->data.func->expr=dup_expr(expr->data.func->expr);
                  break;
      case COORD: n->data.coord=new_coord();
                  n->data.coord->name=expr->data.coord->name;
                  n->data.coord->indx=dup_indel(expr->data.coord->indx);
                  n->data.coord->grd=expr->data.coord->grd;
                  break;
      case GFUNC: n->data.gfunc=new_gfunc();
                  n->data.gfunc->toff=expr->data.gfunc->toff;
                  n->data.gfunc->name=expr->data.gfunc->name;
                  n->data.gfunc->indx=dup_indx(expr->data.gfunc->indx);
                  n->data.gfunc->grd=expr->data.gfunc->grd;
                  break;
    }
    n->left=dup_expr(expr->left);
    n->right=dup_expr(expr->right);
    return n;
  }else return(NULL); 
}

void del_expr(node *expr)
{
  if(!expr)
    return;
  
  del_expr(expr->left);
  
  if(expr->type==D_OP){
    del_expr(expr->data.deriv->expr);
    free(expr->data.deriv);
  }else if(expr->type==FUNC){
    del_expr(expr->data.func->expr);
    free(expr->data.func);
  }else if(expr->type==GFUNC){
    free(expr->data.gfunc);
  }else if(expr->type==COORD){
    free(expr->data.coord);
  }
  
  del_expr(expr->right);
  
  free(expr);
}

/* expands derivatives symbolicaly */
void expand_expr(node *expr)
{
  /* breadth-first traversal of expression parse tree */
  qheader *qn;
  node *ex;
  
  qn=init_queue();
  enqueue(qn,expr);
  while((ex=dequeue(qn))!=NULL) {
    if(ex->type==D_OP && ex->data.deriv->expand && ex->data.deriv->expr){
      node *p;
      
      p=ex->data.deriv->expr;
      if(ex->data.deriv->clst->rank>1){
        node *de;
        
        de=new_node();
        de->type=D_OP;
        de->data.deriv=new_dop();
        de->data.deriv->expr=p;
        de->data.deriv->expand=TRUE;
        de->data.deriv->name=ex->data.deriv->name;
        de->data.deriv->clst=new_coord_tab();
        de->data.deriv->clst->rank=ex->data.deriv->clst->rank-1;
        de->data.deriv->clst->c_names=ex->data.deriv->clst->c_names->next;
        ex->data.deriv->expr=de;
        ex->data.deriv->clst->rank=1;
        ex->data.deriv->clst->c_names->next=NULL;
        expand_expr(de);
        p=de;
      }
      if(p->type==COORD){
        if(p->data.coord->name==ex->data.deriv->clst->c_names->name){
          del_expr(p);
          free(ex->data.deriv);
          ex->type=NUM;
          ex->data.num=1.0;
        }else{
          del_expr(p);
          free(ex->data.deriv);
          ex->type=NUM;
          ex->data.num=0.0;
        }
      }else if(p->type==IDEN && is_coord(p->data.name)){
        if(p->data.name==ex->data.deriv->clst->c_names->name){
          del_expr(p);
          free(ex->data.deriv);
          ex->type=NUM;
          ex->data.num=1.0;
        }else{
          del_expr(p);
          free(ex->data.deriv);
          ex->type=NUM;
          ex->data.num=0.0;
        }
      }else if(p->type==OP){
        switch(p->data.op){
          case '*' : {
                     node *nd1,*nd2,*nt;
      
                     nd1=new_node();
                     nt=new_node();
                     nd2=new_node();
                     *nd1=*ex;
                     nd1->data.deriv->expr=dup_expr(p->right);
                     ex->type=OP;
                     ex->data.op='+';
                     ex->left=p;
                     ex->right=nt;
                     nt->type=OP;
                     nt->data.op='*';
                     nt->left=p->right;
                     nt->right=nd2;
                     nd2->type=D_OP;
                     nd2->data.deriv=new_dop();
                     nd2->data.deriv->expand=TRUE;
                     nd2->data.deriv->name=nd1->data.deriv->name;
                     nd2->data.deriv->clst=dup_ctab(nd1->data.deriv->clst);
                     nd2->data.deriv->expr=dup_expr(p->left);
                     p->right=nd1;
                   }
                   break;
          case '/' : {
                     node *nd1,*nd2,*nm,*t1,*t2;
  
                     nd1=new_node();
                     nd2=new_node();
                     nm=new_node();
                     t1=new_node();
                     t2=new_node();
                     *nd1=*ex;
                     nd1->data.deriv->expr=dup_expr(p->right);
                     nd2->type=D_OP;
                     nd2->data.deriv=new_dop();
                     nd2->data.deriv->expand=TRUE;
                     nd2->data.deriv->name=nd1->data.deriv->name;
                     nd2->data.deriv->clst=dup_ctab(nd1->data.deriv->clst);
                     nd2->data.deriv->expr=dup_expr(p->left);
                     nm->type=OP;
                     nm->data.op='-';
                     nm->left=t1;
                     nm->right=t2;
                     t1->type=OP;
                     t1->data.op='*';
                     t1->left=p->right;
                     t1->right=nd2;
                     t2->type=OP;
                     t2->data.op='*';
                     t2->left=p->left;
                     t2->right=nd1;
                     *ex=*p;
                     p->data.op='*';
                     p->left=dup_expr(ex->right);
                     p->right=dup_expr(ex->right);
                     ex->left=nm;
                     ex->right=p;
                   }
                   break;
          case '+' :
          case '-' : {
                     node *nd;
                   
                     nd=new_node();
                     *nd=*ex;
                     nd->data.deriv->expr=p->left;
                     *ex=*p;
                     ex->left=nd;
                     ex->right=p;
                     p->type=D_OP;
                     p->data.deriv=new_dop();
                     p->data.deriv->expand=TRUE;
                     p->data.deriv->name=nd->data.deriv->name;
                     p->data.deriv->clst=dup_ctab(nd->data.deriv->clst);
                     p->data.deriv->expr=p->right;
                     p->left=p->right=NULL;
                   }
                   break;
          case '^' : {
                     node *t2,*t3,*pl,*d,*de1,*de2,*f;
                     
                     t2=new_node();
                     t3=new_node();
                     pl=new_node();
                     d=new_node();
                     de1=new_node();
                     de2=new_node();
                     f=new_node();
                     *de1=*ex;
                     de1->data.deriv->expr=dup_expr(p->left);
                     ex->type=OP;
                     ex->data.op='*';
                     ex->left=p;
                     ex->right=pl;
                     t2->type=OP;
                     t2->data.op='*';
                     t2->left=d;
                     t2->right=de1;
                     t3->type=OP;
                     t3->data.op='*';
                     t3->left=de2;
                     t3->right=f;
                     pl->type=OP;
                     pl->data.op='+';
                     pl->left=t2;
                     pl->right=t3;
                     d->type=OP;
                     d->data.op='/';
                     d->left=dup_expr(p->right);
                     d->right=dup_expr(p->left);
                     de2->type=D_OP;
                     de2->data.deriv=new_dop();
                     de2->data.deriv->expand=TRUE;
                     de2->data.deriv->name=de1->data.deriv->name;
                     de2->data.deriv->clst=dup_ctab(de1->data.deriv->clst);
                     de2->data.deriv->expr=dup_expr(p->right);
                     f->type=FUNC;
                     f->data.func=new_func();
                     f->data.func->name=Ln;
                     f->data.func->expr=dup_expr(p->left);
                   }
                   break;
        }
      }else if(p->type==FUNC){
        if(p->data.func->name==Ln){
          node *d,*one;
          d=new_node();
          one=new_node();
          one->type=NUM;
          one->data.num=1;
          d->type=OP;
          d->data.op='/';
          d->left=one;
          d->right=p->data.func->expr;
          *p=*ex;
          p->data.deriv->expr=dup_expr(d->right);
          ex->type=OP;
          ex->data.op='*';
          ex->left=d;
          ex->right=p;
        }else if(p->data.func->name==Exp){
          node *d;
          d=new_node();
          *d=*ex;
          d->data.deriv->expr=dup_expr(p->data.func->expr);
          ex->type=OP;
          ex->data.op='*';
          ex->left=p;
          ex->right=d;
        }else if(p->data.func->name==Sin){
          node *d;
          d=new_node();
          *d=*ex;
          d->data.deriv->expr=dup_expr(p->data.func->expr);
          ex->type=OP;
          ex->data.op='*';
          ex->left=p;
          ex->right=d;
          p->data.func->name=Cos;
        }else if(p->data.func->name==Cos){
          node *d,*m;
          d=new_node();
          m=new_node();
          m->type=OP;
          m->data.op='-';
          m->right=p;
          *d=*ex;
          d->data.deriv->expr=dup_expr(p->data.func->expr);
          ex->type=OP;
          ex->data.op='*';
          ex->left=m;
          ex->right=d;
          p->data.func->name=Sin;
        }else if(p->data.func->name==Sqrt){
          node *d,*m,*r;
          d=new_node();
          m=new_node();
          r=new_node();
          d->type=OP;
          d->data.op='/';
          d->left=new_node();
          d->left->type=INUM;
          d->left->data.num=1;
          d->right=p;
          *r=*ex;
          r->data.deriv->expr=dup_expr(p->data.func->expr);
          m->type=OP;
          m->data.op='*';
          m->right=r;
          m->left=d;
          ex->type=OP;
          ex->data.op='*';
          ex->left=new_node();
          ex->left->type=NUM;
          ex->left->data.num=.5;
          ex->right=m;
        }else if(p->data.func->name==Tan){
          node *d,*e,*m,*n;
          d=new_node();
          e=new_node();
          m=new_node();
          n=new_node();
          n->type=INUM;
          n->data.inum=2;
          m->type=OP;
          m->data.op='-';
          m->right=n;
          e->type=OP;
          e->data.op='^';
          e->left=p;
          e->right=m;
          *d=*ex;
          d->data.deriv->expr=dup_expr(p->data.func->expr);
          ex->type=OP;
          ex->data.op='*';
          ex->left=e;
          ex->right=d;
          p->data.func->name=Cos;
        }else if(p->data.func->name==Sinh){
          node *d;
          d=new_node();
          *d=*ex;
          d->data.deriv->expr=dup_expr(p->data.func->expr);
          ex->type=OP;
          ex->data.op='*';
          ex->left=p;
          ex->right=d;
          p->data.func->name=Cosh;
        }else if(p->data.func->name==Cosh){
          node *d;
          d=new_node();
          *d=*ex;
          d->data.deriv->expr=dup_expr(p->data.func->expr);
          ex->type=OP;
          ex->data.op='*';
          ex->left=p;
          ex->right=d;
          p->data.func->name=Sinh;
        }else if(p->data.func->name==Tanh){
          node *d,*e,*m,*n;
          d=new_node();
          e=new_node();
          m=new_node();
          n=new_node();
          n->type=INUM;
          n->data.inum=2;
          m->type=OP;
          m->data.op='-';
          m->right=n;
          e->type=OP;
          e->data.op='^';
          e->left=p;
          e->right=m;
          *d=*ex;
          d->data.deriv->expr=dup_expr(p->data.func->expr);
          ex->type=OP;
          ex->data.op='*';
          ex->left=e;
          ex->right=d;
          p->data.func->name=Cosh;
        }else if(p->data.func->name==ASin){
          node *u,*d,*s,*e,*m,*o,*o1,*t;
          u=p->data.func->expr;
          d=new_node();
          s=new_node();
          e=new_node();
          m=new_node();
          o=new_node();
          t=new_node();
          t->type=INUM;
          t->data.inum=2;
          o->type=INUM;
          o->data.inum=1;
          o1=dup_expr(o);
          e->type=OP;
          e->data.op='^';
          e->left=u;
          e->right=t;
          m->type=OP;
          m->data.op='-';
          m->left=o1;
          m->right=e;
          p->data.func->name=Sqrt;
          p->data.func->expr=m;
          s->type=OP;
          s->data.op='/';
          s->left=o;
          s->right=p;
          *d=*ex;
          d->data.deriv->expr=dup_expr(u);
          ex->type=OP;
          ex->data.op='*';
          ex->left=s;
          ex->right=d;
        }else if(p->data.func->name==ACos){
          node *u,*d,*s,*e,*m,*o,*o1,*t;
          u=p->data.func->expr;
          d=new_node();
          s=new_node();
          e=new_node();
          m=new_node();
          o=new_node();
          o1=new_node();
          t=new_node();
          t->type=INUM;
          t->data.inum=2;
          o->type=INUM;
          o->data.inum=-1;
          o1->type=INUM;
          o1->data.inum=1;
          e->type=OP;
          e->data.op='^';
          e->left=u;
          e->right=t;
          m->type=OP;
          m->data.op='-';
          m->left=o1;
          m->right=e;
          p->data.func->name=Sqrt;
          p->data.func->expr=m;
          s->type=OP;
          s->data.op='/';
          s->left=o;
          s->right=p;
          *d=*ex;
          d->data.deriv->expr=dup_expr(u);
          ex->type=OP;
          ex->data.op='*';
          ex->left=s;
          ex->right=d;
        }else if(p->data.func->name==ATan){
          node *u,*d,*s,*e,*o,*o1,*t;
          u=p->data.func->expr;
          d=new_node();
          s=new_node();
          e=new_node();
          o=new_node();
          t=new_node();
          t->type=INUM;
          t->data.inum=2;
          o->type=INUM;
          o->data.inum=1;
          o1=dup_expr(o);
          e->type=OP;
          e->data.op='^';
          e->left=u;
          e->right=t;
          p->type=OP;
          p->data.op='+';
          p->left=o1;
          p->right=e;
          s->type=OP;
          s->data.op='/';
          s->left=o;
          s->right=p;
          *d=*ex;
          d->data.deriv->expr=dup_expr(u);
          ex->type=OP;
          ex->data.op='*';
          ex->left=s;
          ex->right=d;
        }else if(p->data.func->name==Abs){
          /* derivative is undefined */
        }
      }
    }
    enqueue(qn,ex->left);
    enqueue(qn,ex->right);
  }
}

void expand_ifstat(ifstat *ifst)
{
  int done=0;
  ifstat *is;
  is=ifst;
  do{
    expand_expr(is->lexpr);
    expand_expr(is->expr);
    if(is->etype==0){
      done=1;
    }else if(is->etype==1){
      expand_expr(is->elst.expr);
      done=1;
    }else if(is->etype==2){
      is=is->elst.ifst;
    }
  }while(!done);
}

/* substitutes operator expressions for derivatives
   turns idens into gfuncs and coords */
void evaluate_expr(node *expr)
{
  if(!expr)
    return;
  
  evaluate_expr(expr->left);
  evaluate_expr(expr->right);
  
  switch(expr->type){
    case IDEN : /* change grid funcs to gfuncs and coordinates to coords */
                if(is_coord(expr->data.name)){
                  name_list *n;
                  int i,g;
                  n=expr->data.name;
                  expr->type=COORD;
                  expr->data.coord=new_coord();
                  expr->data.coord->name=n;
                  expr->data.coord->indx=new_indel();
                  expr->data.coord->indx->type=OFFS;
                  expr->data.coord->indx->val.offset=0; /* center at i */
                  for(i=0;i<ncoords && !in_clst(n,&coords[i]);i++);
                  for(g=0;g<ngrids && grids[g].crds!=&coords[i];g++);
                  if(g==ngrids){
                    fprintf(stderr,"No grid defined on coordinates %s.\n",coords[i].name->n);
                    fatal_error("Specification error.");
                  }
                  expr->data.coord->grd=&grids[g]; /* use 1st grid on coord sys */
                }else{
                  gfunc_table *gf;

                  gf=name_to_gfunc(expr->data.name);
                  if(gf){ /* iden is a grid gfunction */
                    index_list *o;
                    int j;
                    expr->type=GFUNC;
                    expr->data.gfunc=new_gfunc();
                    expr->data.gfunc->name=gf->fname;
                    expr->data.gfunc->grd=gf->grd;
                    if(gf->ntlevs==1 && gf->tlev->offset==FLAG){
                      expr->data.gfunc->toff=FLAG;
                    }else{
                      expr->data.gfunc->toff=0; /* center at (n,i) */
                    }
                    expr->data.gfunc->indx=new_index_list();
                    expr->data.gfunc->indx->ind=new_indel();
                    expr->data.gfunc->indx->ind->type=OFFS;
                    expr->data.gfunc->indx->ind->val.offset=0;
                    for(o=expr->data.gfunc->indx,j=1;j<gf->grd->reg.rank;j++){
                      o->next=new_index_list();
                      o->next->ind=new_indel();
                      o->next->ind->type=OFFS;
                      o->next->ind->val.offset=0;
                      o=o->next;
                    }
                    o->next=NULL;
                  }
                }
                break;
    case GFUNC: { /* set grid pointer */
                  index_list *o;
                  gfunc_table *gf;
                  gf=name_to_gfunc(expr->data.gfunc->name);
                  if(gf){ /* gfunc really is a grid gfunction */
                    expr->data.gfunc->grd=gf->grd; /* point to grid */
                    if(gf->ntlevs==1 && gf->tlev->offset==FLAG)
                      expr->data.gfunc->toff=FLAG;
                    for(o=expr->data.gfunc->indx;o!=NULL;o=o->next){
                      if(o->ind->type==EXPR)
                        evaluate_expr(o->ind->val.expr);
                    }
                  }
                }
                break;
    case COORD: { /* set grid pointer */
                  if(is_coord(expr->data.coord->name)){ /* really is a coordinate */
                    int i,g;
                    for(i=0;i<ncoords && !in_clst(expr->data.coord->name,&coords[i]);i++);
                    for(g=0;g<ngrids && grids[g].crds!=&coords[i];g++);
                    if(g==ngrids){
                      fprintf(stderr,"No grid defined on coordinates %s.\n",coords[i].name->n);
                      fatal_error("Specification error.");
                    }
                    expr->data.coord->grd=&grids[g]; /* use 1st grid */
                    if(expr->data.coord->indx->type==EXPR)
                      evaluate_expr(expr->data.coord->indx->val.expr);
                  }
                }
                break;
    case D_OP : 
#ifdef RNPLDEBUG
fprintf(stderr,"eval: expr=");
ex_to_string(stderr,expr);
fprintf(stderr,"\n");
#endif
                evaluate_expr(expr->data.deriv->expr);
#ifdef RNPLDEBUG
fprintf(stderr,"eval: evaluated deriv->expr.  expr=");
ex_to_string(stderr,expr);
fprintf(stderr,"\n");
#endif
                if(!expr->data.deriv->expr){
                  free(expr->data.deriv);
                  expr->type=NUM;
                  expr->data.num=0;
                }else switch(expr->data.deriv->expr->type){
                  case NUM  :
                  case INUM : free(expr->data.deriv->expr);
                              free(expr->data.deriv);
                              expr->type=NUM;
                              expr->data.num=0;
                              break;
                  case IDEN : {
                                dop_table *dp;
                                
                                dp=find_dop(expr->data.deriv->name,expr->data.deriv->clst);
                                if(!dp){
                                  fprintf(stderr,"Can't find <%s> in table\n",expr->data.deriv->name->n);
                                  fatal_error("Can't find derivative operator.");
                                }
                                if(dp->expr->data.name==expr->data.deriv->expr->data.name){
                                  /* expanding recursive derivative operators */
                                  index_list *o;
                                  node *ep=expr->data.deriv->expr;
                                  int j;
                                  ep->type=GFUNC;
                                  ep->data.gfunc=new_gfunc();
                                  ep->data.gfunc->name=dp->expr->data.name;
                                  ep->data.gfunc->grd=NULL;
                                  ep->data.gfunc->toff=0; /* center at (n,i) */
                                  ep->data.gfunc->indx=new_index_list();
                                  ep->data.gfunc->indx->ind=new_indel();
                                  ep->data.gfunc->indx->ind->type=OFFS;
                                  ep->data.gfunc->indx->ind->val.offset=0;
                                  for(o=ep->data.gfunc->indx,j=1;j<dp->ndim;j++){
                                    o->next=new_index_list();
                                    o->next->ind=new_indel();
                                    o->next->ind->type=OFFS;
                                    o->next->ind->val.offset=0;
                                    o=o->next;
                                  }
                                  o->next=NULL;
                                  evaluate_expr(expr);
                                }else{
                                  /* iden must be a constant */
                                  free(expr->data.deriv->expr);
                                  free(expr->data.deriv);
                                  expr->type=NUM;
                                  expr->data.num=0;
                                }
                              }
                              break;
                  case GFUNC: 
                  case COORD: 
                  case OP   : 
                  case FUNC : {
                                node *n,*sv;
                                grid_table *grd;
                                dop_table *dp;
                                
                                sv=expr->data.deriv->expr;
                                dp=find_dop(expr->data.deriv->name,expr->data.deriv->clst); /* find d_op in table */
                                if(!dp){
                                  fprintf(stderr,"Can't find <%s> in table\n",expr->data.deriv->name->n);
                                  fatal_error("Can't find derivative operator.");
                                }
                                n=dup_expr(dp->op_expr);
                                expr_subst(sv,n,dp->expr->data.name);
                                free(expr->data.deriv);
                                *expr=*n;
                                free(n);
                                del_expr(sv);
                                evaluate_expr(expr);
                                grd=get_grid(expr);
                                if(!grd)
                                  grd=&grids[0]; /* use first grid */
                                set_grid(grd,expr);
                              }
                              break;
                }
#ifdef RNPLDEBUG
fprintf(stderr,"eval: evaluated expr.  expr=");
ex_to_string(stderr,expr);
fprintf(stderr,"\n");
#endif
                break;
    case FUNC : evaluate_expr(expr->data.func->expr);
                break;
  }
}

void evaluate_ifstat(ifstat *ifst)
{
  int done=0;
  ifstat *is;
  is=ifst;
  do{
    evaluate_expr(is->lexpr);
    evaluate_expr(is->expr);
    if(is->etype==0){
      done=1;
    }else if(is->etype==1){
      evaluate_expr(is->elst.expr);
      done=1;
    }else if(is->etype==2){
      is=is->elst.ifst;
    }
  }while(!done);
}

void offset_expr(const int toff, offset_type *ind, node *expr)
{
  if(!expr || (toff==0 && ind==NULL))
    return;
    
  offset_expr(toff,ind,expr->left);
  offset_expr(toff,ind,expr->right);
  
  if(expr->type==GFUNC){
    offset_type *o;
    index_list *p;
    if(expr->data.gfunc->toff!=FLAG)
      expr->data.gfunc->toff+=toff;
    for(o=ind,p=expr->data.gfunc->indx;o!=NULL && p!=NULL;o=o->next,p=p->next)
      if(p->ind->type==OFFS)
        p->ind->val.offset+=o->offset;
      else offset_expr(toff,ind,p->ind->val.expr);
  }else if(expr->type==COORD){
    offset_type *o;
    int i;
    for(i=0,o=ind;i<rank(expr->data.coord->name);i++,o=o->next);
    if(expr->data.coord->indx->type==OFFS)
      expr->data.coord->indx->val.offset+=o->offset;
    else offset_expr(toff,ind,expr->data.coord->indx->val.expr);
  }else if(expr->type==FUNC){
    offset_expr(toff,ind,expr->data.func->expr);
  }
}

void offset_ifstat(const int toff, offset_type *ind, ifstat *ifst)
{
  int done=0;
  ifstat *is;
  is=ifst;
  do{
    offset_expr(toff,ind,is->lexpr);
    offset_expr(toff,ind,is->expr);
    if(is->etype==0){
      done=1;
    }else if(is->etype==1){
      offset_expr(toff,ind,is->elst.expr);
      done=1;
    }else if(is->etype==2){
      is=is->elst.ifst;
    }
  }while(!done);
}

void simplify_expr(node *expr)
{
  if(!expr)
    return;
  
  simplify_expr(expr->left);
  simplify_expr(expr->right);
  
  if(expr->type==FUNC){
    simplify_expr(expr->data.func->expr);
  }else if(expr->type==GFUNC){
    index_list *in;
    
    for(in=expr->data.gfunc->indx;in;in=in->next){
      if(in->ind->type==EXPR){
        simplify_expr(in->ind->val.expr);
      }
    }
  }else if(expr->type==COORD){
    if(expr->data.coord->indx->type==EXPR){
      simplify_expr(expr->data.coord->indx->val.expr);
    }
  }else if(expr->type==OP && expr->left && expr->right){
    if((expr->left->type==NUM || expr->left->type==INUM) &&
       (expr->right->type==NUM || expr->right->type==INUM)){ /* 2 numbers */
      double d;
      switch(expr->data.op){
        case '+' : d=((expr->left->type==NUM)?expr->left->data.num:expr->left->data.inum) +
                     ((expr->right->type==NUM)?expr->right->data.num:expr->right->data.inum);
                   del_expr(expr->left);
                   del_expr(expr->right);
                   expr->type=NUM;
                   expr->data.num=d;
                   expr->left=expr->right=NULL;
                   break;
        case '-' : d=((expr->left->type==NUM)?expr->left->data.num:expr->left->data.inum) -
                     ((expr->right->type==NUM)?expr->right->data.num:expr->right->data.inum);
                   del_expr(expr->left);
                   del_expr(expr->right);
                   expr->type=NUM;
                   expr->data.num=d;
                   expr->left=expr->right=NULL;
                   break;
        case '*' : d=((expr->left->type==NUM)?expr->left->data.num:expr->left->data.inum) *
                     ((expr->right->type==NUM)?expr->right->data.num:expr->right->data.inum);
                   del_expr(expr->left);
                   del_expr(expr->right);
                   expr->type=NUM;
                   expr->data.num=d;
                   expr->left=expr->right=NULL;
                   break;
        case '/' : if(((expr->right->type==NUM)?expr->right->data.num:expr->right->data.inum)==0){
                     ex_to_string(stderr,expr);
                     fprintf(stderr,"\n");
                     fatal_error("Division by zero.");
                   }
                   d=((expr->left->type==NUM)?expr->left->data.num:expr->left->data.inum) /
                     ((expr->right->type==NUM)?expr->right->data.num:expr->right->data.inum);
                   del_expr(expr->left);
                   del_expr(expr->right);
                   expr->type=NUM;
                   expr->data.num=d;
                   expr->left=expr->right=NULL;
                   break;
        case '^' : d=pow(((expr->left->type==NUM)?expr->left->data.num:(double)expr->left->data.inum),
                     ((expr->right->type==NUM)?expr->right->data.num:(double)expr->right->data.inum));
                   del_expr(expr->left);
                   del_expr(expr->right);
                   expr->type=NUM;
                   expr->data.num=d;
                   expr->left=expr->right=NULL;
                   break;
      }   
    }else if((expr->left->type==IDEN && expr->right->type==IDEN) ||
             (expr->left->type==GFUNC && expr->right->type==GFUNC) ||
             (expr->left->type==COORD && expr->right->type==COORD) ||
             (expr->left->type==FUNC && expr->right->type==FUNC)){ /* 2 names */
      char  *l,*r;
      switch(expr->left->type){
        case IDEN  : l=expr->left->data.name->n;
                     r=expr->right->data.name->n;
                     break;
        case GFUNC : l=expr->left->data.gfunc->name->n;
                     r=expr->right->data.gfunc->name->n;
                     break;
        case COORD : l=expr->left->data.coord->name->n;
                     r=expr->right->data.coord->name->n;
                     break;
        case FUNC  : l=expr->left->data.func->name->n;
                     r=expr->right->data.func->name->n;
                     break;
      }
      if(mystrcmp(l,r)>0){ /* out of order */
        node *tmp;
        
        switch(expr->data.op){
          case '*' :
          case '+' : tmp=expr->left;
                     expr->left=expr->right;
                     expr->right=tmp;
                     break;
        }
      }
    }else if((expr->left->type==NUM && expr->left->data.num==0) ||
       (expr->left->type==INUM && expr->left->data.inum==0)){
      switch(expr->data.op){
        case '+' : del_expr(expr->left);
                   *expr=*expr->right;
                   break;
        case '-' : del_expr(expr->left);
                 if(expr->right->type==NUM){
                   *expr=*expr->right;
                   expr->data.num=-expr->data.num;
                   }else if(expr->right->type==INUM){
                     *expr=*expr->right;
                     expr->data.inum=-expr->data.inum;
                   }else expr->left=NULL;
                   break;
        case '*' : del_expr(expr->left);
                   del_expr(expr->right);
                   expr->left=expr->right=NULL;
                   expr->type=NUM;
                   expr->data.num=0;
                   break;
        case '^' : if(!(expr->right->type==NUM && expr->right->data.num==0) &&
                      !(expr->right->type==INUM && expr->right->data.inum==0)){
                     del_expr(expr->left);
                     del_expr(expr->right);
                     expr->left=expr->right=NULL;
                     expr->type=NUM;
                     expr->data.num=0;
                   } else {
                     del_expr(expr->left);
                     del_expr(expr->right);
                     expr->left=expr->right=NULL;
                     expr->type=NUM;
                     expr->data.num=1;
                   }
                   break;
        case '/' : if(!(expr->right->type==NUM && expr->right->data.num==0) &&
                      !(expr->right->type==INUM && expr->right->data.inum==0)){
                     del_expr(expr->left);
                     del_expr(expr->right);
                     expr->left=expr->right=NULL;
                     expr->type=NUM;
                     expr->data.num=0;
                   }else fatal_error("0/0 is undefined.");
                   break;
      }
    }else if((expr->right->type==NUM && expr->right->data.num==0) ||
            (expr->right->type==INUM && expr->right->data.inum==0)){
      switch(expr->data.op){
        case '-' :
        case '+' : del_expr(expr->right);
                   *expr=*expr->left;
                   break;
        case '*' : del_expr(expr->left);
                   del_expr(expr->right);
                   expr->left=expr->right=NULL;
                   expr->type=NUM;
                   expr->data.num=0;
                   break;
        case '/' : ex_to_string(stderr,expr);
                   fprintf(stderr,"\n");
                   fatal_error("Division by zero.");
                   break;
        case '^' : del_expr(expr->left);
                   del_expr(expr->right);
                   expr->left=expr->right=NULL;
                   expr->type=NUM;
                   expr->data.num=1;
                   break;
       }
    }else if((expr->left->type==NUM && expr->left->data.num==1) ||
       (expr->left->type==INUM && expr->left->data.inum==1)){
      switch(expr->data.op){
        case '*' : del_expr(expr->left);
                   *expr=*expr->right;
                   break;
        case '^' : del_expr(expr->left);
                   del_expr(expr->right);
                   expr->left=expr->right=NULL;
                   expr->type=NUM;
                   expr->data.num=1;
                   break;
      }
    }else if((expr->right->type==NUM && expr->right->data.num==1) ||
            (expr->right->type==INUM && expr->right->data.inum==1)){
      switch(expr->data.op){
        case '*' : 
        case '/' : 
        case '^' : del_expr(expr->right);
                   *expr=*expr->left;
                   break;
       }
    }
    switch(expr->data.op){
      case '+' : if(expr->left && compare_expr(expr->left,expr->right)){
                   del_expr(expr->left);
                   expr->left=new_node();
                   expr->left->type=INUM;
                   expr->left->data.inum=2;
                   expr->left->left=NULL;
                   expr->left->right=NULL;
                   expr->data.op='*';
                 }else if(expr->left && expr->left->type==OP){
                   switch(expr->left->data.op){
                     case '*' : if(compare_expr(expr->right,expr->left->left)){
                                  del_expr(expr->left->left);
                                  expr->left->left=new_node();
                                  expr->left->left->type=INUM;
                                  expr->left->left->data.inum=1;
                                  expr->left->left->left=NULL;
                                  expr->left->left->right=NULL;
                                  expr->left->data.op='+';
                                  expr->data.op='*';
                                  simplify_expr(expr->left);
                                }else if(compare_expr(expr->right,expr->left->right)){
                                  del_expr(expr->left->right);
                                  expr->left->right=new_node();
                                  expr->left->right->type=INUM;
                                  expr->left->right->data.inum=1;
                                  expr->left->right->left=NULL;
                                  expr->left->right->right=NULL;
                                  expr->left->data.op='+';
                                  expr->data.op='*';
                                  simplify_expr(expr->left);
                               }
                               break;
                     case '/' :
                     case '+' :
                     case '-' :
                     case '^' :
                               break;
                   }
                 }else if(expr->right->type==OP){
                   switch(expr->right->data.op){
                     case '*' : if(compare_expr(expr->left,expr->right->left)){
                                  del_expr(expr->right->left);
                                  expr->right->left=new_node();
                                  expr->right->left->type=INUM;
                                  expr->right->left->data.inum=1;
                                  expr->right->left->left=NULL;
                                  expr->right->left->right=NULL;
                                  expr->right->data.op='+';
                                  expr->data.op='*';
                                  simplify_expr(expr->right);
                                }else if(compare_expr(expr->left,expr->right->right)){
                                  del_expr(expr->right->right);
                                  expr->right->right=new_node();
                                  expr->right->right->type=INUM;
                                  expr->right->right->data.inum=1;
                                  expr->right->right->left=NULL;
                                  expr->right->right->right=NULL;
                                  expr->right->data.op='+';
                                  expr->data.op='*';
                                  simplify_expr(expr->right);
                                }
                                break;
                     case '/' :
                     case '+' :
                     case '-' :
                     case '^' :
                                break;
                   }
                 }
                 break;
      case '-' :
      case '*' :
      case '/' :
      case '^' :
                 break;
    }
  }                       
}

void simplify_ifstat(ifstat *ifst)
{
  int done=0;
  ifstat *is;
  is=ifst;
  do{
    simplify_expr(is->expr);
    if(is->etype==0){
      done=1;
    }else if(is->etype==1){
      simplify_expr(is->elst.expr);
      done=1;
    }else if(is->etype==2){
      is=is->elst.ifst;
    }
  }while(!done);
}

void make_expand(node *expr)
{
  if(!expr)
    return;
  make_expand(expr->left);
  make_expand(expr->right);
  
  if(expr->type==D_OP){
    expr->data.deriv->expand=1;
  }
}

int check_expr(node *expr)
{
  int m=1,l=1,r=1;

  if(expr){
    switch(expr->type){
      case IDEN    :  m=name_type(expr->data.name);
                    if(m==-1){
                      fprintf(stderr,"Error: %s is undeclared.\n",expr->data.name->n);
                      m=0;
                    }else if(m!=PARAM && m!=CONST && m!=CDIF && m!=GBASE){
                      fprintf(stderr,"Error: %s cannot appear in a residual.\n",expr->data.name->n);
                      show_name_type(expr->data.name);
                      m=0;
                    }else m=1;
                    break;
      case D_OP    :  m=name_type(expr->data.deriv->name);
                    if(m==-1){
                      fprintf(stderr,"Error: %s is undeclared.\n",expr->data.deriv->name->n);
                      m=0;
                    }else if(m!=D_OP){
                      fprintf(stderr,"Error: %s is mascarading as a D_OP.\n",expr->data.deriv->name->n);
                      show_name_type(expr->data.deriv->name);
                      m=0;
                    }else m=1;
                    break;
      case GFUNC  :  m=name_type(expr->data.gfunc->name);
                    if(m==-1){
                      fprintf(stderr,"Error: %s is undeclared.\n",expr->data.gfunc->name->n);
                      m=0;
                    }else if(m!=GFUNC){
                      fprintf(stderr,"Error: %s is mascarading as a grid function.\n",
                              expr->data.gfunc->name->n);
                      show_name_type(expr->data.gfunc->name);
                      m=0;
                    }else m=1;
                    break;
      case FUNC    :  m=name_type(expr->data.func->name);
                    if(m==-1){
                      fprintf(stderr,"Error: %s is undeclared.\n",expr->data.func->name->n);
                      m=0;
                    }else if(m!=FUNC){
                      fprintf(stderr,"Error: %s is mascarading as a function.\n",expr->data.func->name->n);
                      show_name_type(expr->data.func->name);
                      m=0;
                    }else m=1;
                    break;
      case COORD  :  m=name_type(expr->data.coord->name);
                    if(m==-1){
                      fprintf(stderr,"Error: %s is undeclared.\n",expr->data.coord->name->n);
                      m=0;
                    }else if(m!=COORD){
                      fprintf(stderr,"Error: %s is mascarading as a coordinate.\n",
                              expr->data.coord->name->n);
                      show_name_type(expr->data.coord->name);
                      m=0;
                    }else m=1;
                    break;
    }
    l=check_expr(expr->left);
    r=check_expr(expr->right);
  }
  return r*l*m;
}

int check_ifstat(ifstat *ifst)
{
  int done=0,res;
  ifstat *is;
  is=ifst;
  do{
    res=check_expr(is->lexpr);
    res*=check_expr(is->expr);
    if(is->etype==0){
      done=1;
    }else if(is->etype==1){
      res*=check_expr(is->elst.expr);
      done=1;
    }else if(is->etype==2){
      is=is->elst.ifst;
    }
  }while(!done);
  return(res);
}

node *deriv_wrt_gfunc(node *expr, node *gf)
{
  node *d;
  
  d=new_node();
  d->type=D_OP;
  d->data.deriv=new_dop();
  d->data.deriv->expand=TRUE;
  d->data.deriv->name=dopers[0].name;
  d->data.deriv->clst=new_coord_tab();
  d->data.deriv->clst->rank=1;
  d->data.deriv->clst->c_names=new_coord_list();
/*  d->data.deriv->clst->c_names->name=coords[0].c_names->name; */
  d->data.deriv->clst->c_names->name=gf->data.gfunc->name;
  d->data.deriv->expr=dup_expr(expr);
  expand_expr(d);
  eval_wrt_gfunc(d,gf);
  simplify_expr(d);
  return d;
}

void eval_wrt_gfunc(node *expr, node *gf)
{
  if(!expr)
    return;
  
  eval_wrt_gfunc(expr->left,gf);
  
  switch(expr->type){
    case D_OP : if(!expr->data.deriv->expr){
                  free(expr->data.deriv);
                  expr->type=NUM;
                  expr->data.num=0;
                }else switch(expr->data.deriv->expr->type){
                  case IDEN :
                  case COORD:
                  case NUM  :
                  case INUM : free(expr->data.deriv->expr);
                              free(expr->data.deriv);
                              expr->type=NUM;
                              expr->data.num=0;
                              break;
                  case GFUNC: if(compare_expr(expr->data.deriv->expr,gf)){
                                free(expr->data.deriv->expr);
                                free(expr->data.deriv);
                                expr->type=NUM;
                                expr->data.num=1;
                              }else{
                                free(expr->data.deriv->expr);
                                free(expr->data.deriv);
                                expr->type=NUM;
                                expr->data.num=0;
                              }
                              break;
                }
                break;
  }
  
  eval_wrt_gfunc(expr->right,gf);
}

/* return grid from any grid function in d_op (all should be equal) */
grid_table *get_grid(node *ex)
{
  grid_table *g;
  
  if(!ex)
    return(NULL);

  g=get_grid(ex->left);

  if(!g)
    if(ex->type==GFUNC)
      g=ex->data.gfunc->grd;
    else if(ex->type==FUNC)
      g=get_grid(ex->data.func->expr);
  
  if(!g)
    g=get_grid(ex->right);

  return(g);
}

/* set the grid of any coord to grd */
void set_grid(grid_table *grd, node *ex)
{
  if(!ex)
    return;
  
  set_grid(grd,ex->left);
  set_grid(grd,ex->right);
  
  if(ex->type==COORD)
    ex->data.coord->grd=grd;
}

void name_subst(name_list *nm, grid_table *grd, node *ex, const name_list *s_nm)
{
  if(!ex)
    return;
  
  name_subst(nm,grd,ex->left, s_nm);
  
  if(ex->type==GFUNC && ex->data.gfunc->name==s_nm){
    ex->data.gfunc->name=nm;
    ex->data.gfunc->grd=grd;
  }
  
  name_subst(nm,grd,ex->right, s_nm);
}

/* I don't know why these are here, they are never called
void coord_subst(coord_ref *crd, node *ex)
{
  if(!ex)
    return;
  
  coord_subst(crd,ex->left);
  
  if(ex->type==GFUNC){
    free(ex->data.gfunc);
    ex->type=COORD;
    ex->data.coord=new_coord();
    *ex->data.coord=*crd;
  }
  
  coord_subst(crd,ex->right);
}

void gfunc_subst(gfunction *gf, node *ex, const name_list *s_nm)
{
  if(!ex)
    return;
  
  gfunc_subst(gf,ex->left,s_nm);
  
  if(ex->type==GFUNC && ex->data.gfunc->name==s_nm){
    offset_type *p,*q;
    ex->data.gfunc->name=gf->name;
    if(ex->data.gfunc->toff!=FLAG)
      ex->data.gfunc->toff+=gf->toff;
    for(p=ex->data.gfunc->indx,q=gf->indx;p!=NULL && q!=NULL;p=p->next,q=q->next)
      p->offset+=q->offset;
    ex->data.gfunc->grd=gf->grd;
  }
  
  gfunc_subst(gf,ex->right,s_nm);
}
*/

/* substitute e1 (from resid) into ex (from operator) */
void expr_subst(node *e1, node *ex, const name_list *s_nm)
{
  /* traverse ex */
  if(!ex)
    return;
  
  expr_subst(e1,ex->left,s_nm);
  expr_subst(e1,ex->right,s_nm);

  if(ex->type==GFUNC && ex->data.gfunc->name==s_nm){
    node *e2;
    
    e2=dup_expr(e1);
    gfunc_stuff(ex,e2);
    *ex=*e2;
    free(e2);
  }
}

/* use the gfunc in e1 to setup gfuncs and coords in ex */
void gfunc_stuff(node *e1, node *ex)
{
  /* traverse ex */
  if(!ex)
    return;
  
  gfunc_stuff(e1,ex->left);
  
  switch(ex->type){
    case GFUNC :  {
                    int i;
                    index_list *p,*q;
                    if(ex->data.gfunc->toff!=FLAG)
                      ex->data.gfunc->toff+=e1->data.gfunc->toff;
                    for(p=ex->data.gfunc->indx,q=e1->data.gfunc->indx;p!=NULL && q!=NULL;p=p->next,q=q->next){
                      if(p->ind->type==OFFS){
                        if(q->ind->type==OFFS){
                          p->ind->val.offset+=q->ind->val.offset;
                        }else if(q->ind->type==EXPR){
                          p->ind->type=EXPR;
                          p->ind->val.expr=dup_expr(q->ind->val.expr);
                        }
                      }else if(p->ind->type==EXPR){
                        if(q->ind->type==OFFS){
                          node *pl,*of;
                          pl=new_node();
                          pl->type=OP;
                          pl->data.op='+';
                          of=new_node();
                          of->type=INUM;
                          of->data.inum=q->ind->val.offset;
                          pl->left=p->ind->val.expr;
                          pl->right=of;
                          p->ind->val.expr=pl;
                        }else if(q->ind->type==EXPR){
                          del_expr(p->ind->val.expr);
                          p->ind->val.expr=dup_expr(q->ind->val.expr);
                        }
                      }
                    }
                  }
                  break;
    case COORD : {
                   int i,r;
                   index_list *p;
                   
                   r=rank(ex->data.coord->name);
                   for(p=e1->data.gfunc->indx,i=0;p!=NULL && i<r;p=p->next,i++);
                   if(p==NULL)
                     fatal_error("gfunc_stuff: Derviative doesn't have dimensions of coordinate system.");
                   if(ex->data.coord->indx->type==OFFS){
                     if(p->ind->type==OFFS){
                        ex->data.coord->indx->val.offset+=p->ind->val.offset;
                     }else if(p->ind->type==EXPR){
                       ex->data.coord->indx->type=EXPR;
                       ex->data.coord->indx->val.expr=dup_expr(p->ind->val.expr);
                     }
                   }else if(ex->data.coord->indx->type==EXPR){
                     if(p->ind->type==OFFS){
                       node *pl,*of;
                          
                       pl=new_node();
                       pl->type=OP;
                       pl->data.op='+';
                       of=new_node();
                       of->type=INUM;
                       of->data.inum=p->ind->val.offset;
                       pl->left=ex->data.coord->indx->val.expr;
                       pl->right=of;
                       ex->data.coord->indx->val.expr=pl;
                     }else if(p->ind->type==EXPR){
                         del_expr(ex->data.coord->indx->val.expr);
                        ex->data.coord->indx->val.expr=dup_expr(p->ind->val.expr);
                     }
                   }
                 }
                 break;
    case FUNC  : gfunc_stuff(e1,ex->data.func->expr);
                 break;
    case D_OP  : fatal_error("gfunc_stuff: How did this get in here?");
                 break;
  }
  
  gfunc_stuff(e1,ex->right);
}

/* adds an offset to the offset list sorted with greatest first */
void add_offset(offset_type *l, offset_type *o)
{
  offset_type *nn,*p,*r;

  if(!l)
    fatal_error("add_offset: input list is NULL\n");
  if(o->offset==l->offset)
    free(o);
  else if(o->offset>l->offset){
    int t;
    o->next=l->next;
    l->next=o;
    t=l->offset;
    l->offset=o->offset;
    o->offset=t;
  } else {
    nn=o;
    p=l;
    while(p->next && nn->offset<p->next->offset)
      p=p->next;
    if(p->next && nn->offset==p->next->offset){
      free(nn);
    } else {
      nn->next=p->next;
      p->next=nn;
    }
  }
}

/* add dop_ref_list o to dop_ref_list l */
void add_dop_ref(dop_ref_list *l, dop_ref_list *o)
{
  dop_ref_list *nn,*p,*r,*q;
  
/* removed 95/01/17 by RLM
  if(o->dop==l->dop){
    gfunc_ref_list *ro,*q;
    for(ro=o->gf_refs;ro!=NULL;ro=q){
      q=ro->next;
      add_gfunc_ref(l->gf_refs,ro);
    }
    free(o);
  }else */
  if(!l)
    fatal_error("add_dop_ref: input list is NULL.\n");
#ifdef RNPLDEBUG
fprintf(stderr,"add_dop_ref(<%s>,<%s>)\n",l->dop->name->n,o->dop->name->n);
#endif
  while(o){
    q=o->next;
#ifdef RNPLDEBUG
fprintf(stderr,"add_dop_ref q=%p\n",q);
#endif
    if(o->dop<=l->dop){
      gfunc_ref_list *grl;
      dop_table *d;
      o->next=l->next;
      l->next=o;
      grl=l->gf_refs;
      d=l->dop;
      l->gf_refs=o->gf_refs;
      l->dop=o->dop;
      o->gf_refs=grl;
      o->dop=d;
    } else {
      nn=o;
      p=l;
      while(p->next && nn->dop>p->next->dop)
        p=p->next;
      nn->next=p->next;
      p->next=nn;
  /* removed 95/07/12 RLM
      if(p->next && nn->dop==p->next->dop){
        gfunc_ref_list *ro,*q;
        for(ro=nn->gf_refs;ro!=NULL;ro=q){
          q=ro->next;
          add_gfunc_ref(p->next->gf_refs,ro);
        }
        free(nn);
      } else { 
        nn->next=p->next;
        p->next=nn;
      } 
  */
    }
    o=q;
  }  
}

/*  add coord_list o to coord_list l.  Sort alphabetically */

void add_name_ref(coord_list *l, coord_list *o)
{
  coord_list *p,*q;
  
  if(!l)
    fatal_error("add_name_ref: input list is NULL.\n");
  while(o){
    q=o->next;
    if(o->name==l->name)
      free(o);
    else if(mystrcmp(o->name->n,l->name->n)<0){ /* swap */
      name_list *n;
      o->next=l->next;
      l->next=o;
      n=l->name;
      l->name=o->name;
      o->name=n;
    }else{
      p=l;
      while(p->next && mystrcmp(o->name->n,p->next->name->n)>0)
        p=p->next;
      if(p->next && o->name==p->next->name){
        free(o);
      } else {
        o->next=p->next;
        p->next=o;
      }
    }
    o=q;
  }
}

/*  add param_ref_list o to param_ref_list l.  Sort alphabetically */

void add_param_ref(param_ref_list *l, param_ref_list *o)
{
  param_ref_list *p,*q;
  
  if(!l)
    fatal_error("add_param_ref: input list is NULL.");
  while(o){
    q=o->next;
    if(o->par->name==l->par->name)
      free(o);
    else if(mystrcmp(o->par->name->n,l->par->name->n)<0){ /* swap */
      param_table *pt;
      o->next=l->next;
      l->next=o;
      pt=l->par;
      l->par=o->par;
      o->par=pt;
    }else{
      p=l;
      while(p->next && mystrcmp(o->par->name->n,p->next->par->name->n)>0)
        p=p->next;
      if(p->next && o->par->name==p->next->par->name){
        free(o);
      } else {
        o->next=p->next;
        p->next=o;
      }
    }
    o=q;
  }
}

/*  add a gfunc_ref_list o to gfunc_ref_list l.  Sort first by grid,
    then alphabetically by gfunc name, then from latest to earliest toff */
    
void add_gfunc_ref(gfunc_ref_list *l, gfunc_ref_list *o)
{
  gfunc_ref_list *p,*q;
  
  if(!l)
    fatal_error("add_gfunc_ref: input list is NULL.\n");
  while(o){
    q=o->next;
    if(o->gfunc==l->gfunc && o->toff==l->toff)
      free(o);
    else if(o->gfunc->grd < l->gfunc->grd || (o->gfunc->grd == l->gfunc->grd && o->gfunc<l->gfunc)
            || (o->gfunc->grd == l->gfunc->grd && o->gfunc==l->gfunc && o->toff>l->toff)){
      int t,a;
      name_list *n;
      gfunc_table *g;
      o->next=l->next;
      l->next=o;
      t=l->toff;
      n=l->name;
      a=l->array;
      g=l->gfunc;
      l->toff=o->toff;
      l->name=o->name;
      l->array=o->array;
      l->gfunc=o->gfunc;
      o->toff=t;
      o->name=n;
      o->array=a;
      o->gfunc=g;
    }else{
      p=l;
      while(p->next && (o->gfunc->grd>p->next->gfunc->grd || 
            (o->gfunc->grd==p->next->gfunc->grd && o->gfunc>p->next->gfunc) || 
            (o->gfunc->grd==p->next->gfunc->grd && o->gfunc==p->next->gfunc && o->toff<p->next->toff)))
        p=p->next;
      if(p->next && o->gfunc==p->next->gfunc && o->toff==p->next->toff){
        free(o);
      } else {
        o->next=p->next;
        p->next=o;
      }
    }
    o=q;
  }
}

/* add parameter o to list l in alphabetical order */
void add_param(param_dec *l, param_dec *o)
{
  param_dec *nn,*p,*r;

  if(!l)
    fatal_error("add_param: input list is NULL.\n");
  if(o->name==l->name)
    free(o);
  else if(mystrcmp(o->name->n,l->name->n)<0){
    int t,d,c;
    v_size *s;
    name_list *n;
    vector v;
    o->next=l->next;
    l->next=o;
    t=l->type;
    s=l->size;
    c=l->con;
    d=l->def;
    n=l->name;
    v=l->def_val;
    l->type=o->type;
    l->size=o->size;
    l->con=o->con;
    l->def=o->def;
    l->name=o->name;
    l->def_val=o->def_val;
    o->type=t;
    o->size=s;
    o->con=c;
    o->def=d;
    o->name=n;
    o->def_val=v;
  } else {
    nn=o;
    p=l;
    while(p->next && mystrcmp(nn->name->n,p->next->name->n)>0)
      p=p->next;
    if(p->next && nn->name==p->next->name){
      free(nn);
    } else {
      nn->next=p->next;
      p->next=nn;
    }
  }
}

/* add attribute to list in alphabetical order */
void add_attrib(attrib_dec *l, attrib_dec *o)
{
  attrib_dec *nn,*p,*r;

  if(!l)
    fatal_error("add_attrib: input list is NULL.\n");
  if(o->name==l->name)
    free(o);
  else if(mystrcmp(o->name->n,l->name->n)<0){
    int t,e,d;
    name_list *n;
    scalar_list *v;
    o->next=l->next;
    l->next=o;
    t=l->type;
    e=l->encoding;
    d=l->def;
    n=l->name;
    v=l->def_val;
    l->type=o->type;
    l->encoding=o->encoding;
    l->def=o->def;
    l->name=o->name;
    l->def_val=o->def_val;
    o->type=t;
    o->encoding=e;
    o->def=d;
    o->name=n;
    o->def_val=v;
  } else {
    nn=o;
    p=l;
    while(p->next && mystrcmp(nn->name->n,p->next->name->n)>0)
      p=p->next;
    if(p->next && nn->name==p->next->name){
      free(nn);
    } else {
      nn->next=p->next;
      p->next=nn;
    }
  }
}

/*  adds work_list o to work_list l.  Entries are sorted by number. */
void add_work_ref(work_list *l, work_list *o)
{
  work_list *p,*q;
  
  if(!l)
    fatal_error("add_work_ref: input list is NULL.\n");
  while(o){
     q=o->next;
     if(o->work->num==l->work->num){
      del_expr(o->work->expr);
      free(o->work);
      free(o);
     }else if(o->work->num<l->work->num){
        work_array *wk;
        o->next=l->next;
        l->next=o;
        wk=l->work;
        l->work=o->work;
        o->work=wk;
     }else{
        for(p=l;p->next && o->work->num>p->next->work->num;p=p->next);
        if(p->next && o->work->num==p->next->work->num){
           del_expr(o->work->expr);
           free(o->work);
           free(o);
        }else{
           o->next=p->next;
           p->next=o;
        }
     }
     o=q;
  }
}

/* n is the expression of a derivative operator.  It shouldn't
   contain and functions, but could.
*/
offset_type *find_tlevs(const node *n)
{
  offset_type *l,*r,*m,*o;
  
  if(!n)
    return NULL;
  
  l=find_tlevs(n->left);
  r=find_tlevs(n->right);

  if(l){
    for(m=r;m!=NULL;m=o){
      o=m->next;
      add_offset(l,m);
    }
  }else l=r;
  
  if(n->type==FUNC){
    r=find_tlevs(n->data.func->expr);
    if(l){
      for(m=r;m!=NULL;m=o){
        o=m->next;
        add_offset(l,m);
      }
    }else l=r;
  }else if(n->type==GFUNC){
    m=new_offset();
    m->offset=n->data.gfunc->toff;
    if(l)
      add_offset(l,m);
    else l=m;
  }
  return l;
}

/* n is the expression of a derivative operator.  It shouldn't
   contain any functions, but could.
*/
/* this currently finds max an min *offset* indicies in an expression.
   it ignores absolute indicies */
bounds *find_indx(const node *n)
{
  bounds *l,*r,*m,*p,*q;
  
  if(!n)
    return NULL;
    
  l=find_indx(n->left);
  r=find_indx(n->right);
  
  if(l && r){
    for(p=l,q=r;p!=NULL;p=p->next){
      if(q->upper > p->upper)
        p->upper=q->upper;
      if(q->lower < p->lower)
        p->lower=q->lower;
      m=q;
      q=q->next;
      free(m);
    }
  }else if(r)
    l=r;
    
  if(n->type==FUNC){
    l=find_indx(n->data.func->expr);
  }else if(n->type==GFUNC){
    index_list *o;
    if(l){
      for(p=l,o=n->data.gfunc->indx;o!=NULL;o=o->next,p=p->next){
        if(o->ind->type==OFFS){
          if(o->ind->val.offset>p->upper)
            p->upper=o->ind->val.offset;
          if(o->ind->val.offset<p->lower)
            p->lower=o->ind->val.offset;
        }
      }
    }else{
      if((o=n->data.gfunc->indx)!=NULL){
        p=l=new_bounds();
        if(o->ind->type==OFFS){
          l->upper=l->lower=o->ind->val.offset;
        }else{
          l->upper=l->lower=0;
        }
        l->next=NULL;
        while(o->next){
          p->next=new_bounds();
          p=p->next;
          o=o->next;
          if(o->ind->type==OFFS){
            p->upper=p->lower=o->ind->val.offset;
          }else{
            p->upper=p->lower=0;
          }
        }
      }
    }
  }
  return l;
}

/* ex is the expression inside a d_op.
*/
gfunc_ref_list *find_gfs(const node *ex)
{
  gfunc_ref_list *l,*r,*m,*n;
  gfunc_table *gf;
  
  if(!ex)
    return NULL;
    
  l=find_gfs(ex->left);
  r=find_gfs(ex->right);
  
  if(l){
    add_gfunc_ref(l,r);
  }else l=r;
  
  if(ex->type==FUNC){
    r=find_gfs(ex->data.func->expr);
    if(l){
      add_gfunc_ref(l,r);
    }else l=r;
  }else if(ex->type==D_OP){
    /* assume this deriv is the same as the outer deriv
       and just return list.  Errors will be caught in
       evaluate_expr */
    r=find_gfs(ex->data.deriv->expr);
    if(l){
      add_gfunc_ref(l,r);
    }else l=r;
  }else if(ex->type==IDEN){
    if((gf=name_to_gfunc(ex->data.name))!=NULL){ /* iden is a grid gfunction */
      m=new_gfunc_ref();
      m->gfunc=gf;
      m->toff=0; /* doesn't matter */
      if(l)
        add_gfunc_ref(l,m);
      else l=m;
    }
  }else if(ex->type==GFUNC){
    if((gf=name_to_gfunc(ex->data.gfunc->name))!=NULL){ /* really is a grid gfunction */
      m=new_gfunc_ref();
      m->gfunc=gf;
      m->toff=0; /* doesn't matter */
      if(l)
        add_gfunc_ref(l,m);
      else l=m;
    }
  }
  return l;
}

/* ex is the expression of a residual.  It has not yet gone
   through the simplification and substitution phases.
*/
dop_ref_list *find_dops(const node *ex)
{
  dop_ref_list *l,*r,*m,*n;
  
#ifdef RNPLDEBUG
fprintf(stderr,"find_dops\n");
#endif
  if(!ex)
    return NULL;
    
  l=find_dops(ex->left);
  r=find_dops(ex->right);
  
  if(l){
    add_dop_ref(l,r);
  }else l=r;
  
  if(ex->type==FUNC){
    r=find_dops(ex->data.func->expr);
    if(l){
      add_dop_ref(l,r);
    }else l=r;
  }else if(ex->type==D_OP){
    int i;
    for(i=0;i<nopers && (ex->data.deriv->name!=dopers[i].name || 
      !compare_ctab(ex->data.deriv->clst,dopers[i].clst));i++); /* find d_op in table */
    if(i==nopers){
      fprintf(stderr,"Can't find %s in table\n",ex->data.deriv->name->n);
      fatal_error("Can't find derivative operator.");
    }
    m=new_dop_ref();
    m->dop=&dopers[i];
    m->gf_refs=find_gfs(ex->data.deriv->expr);
    if(l)
      add_dop_ref(l,m);
    else l=m;
  }
  return l;
}

dop_ref_list *find_dops_if(const ifstat *ifst)
{
  dop_ref_list *dl,*dr,*dm,*dn;
  
  if(!ifst)
    return NULL;
    
  dl=find_dops(ifst->lexpr);
  dr=find_dops(ifst->expr);
  if(dl){
    add_dop_ref(dl,dr);
  }else dl=dr;
  if(ifst->etype==1){
    dr=find_dops(ifst->elst.expr);
    if(dl){
      add_dop_ref(dl,dr);
    }else dl=dr;
  }else if(ifst->etype==2){
    dr=find_dops_if(ifst->elst.ifst);
    if(dl){
      add_dop_ref(dl,dr);
    }else dl=dr;
  }
  return dl;
}

/* ex is the expression of a residual.  It has already gone
   through the simplification and substitution phases.
*/
gfunc_ref_list *find_gfuncs(const node *ex)
{
  gfunc_ref_list *l,*r,*m,*n;
  
  if(!ex)
    return NULL;
  
  l=find_gfuncs(ex->left);
  r=find_gfuncs(ex->right);
  
  if(l){
    add_gfunc_ref(l,r);
  }else l=r;
  
  if(ex->type==FUNC){
    m=find_gfuncs(ex->data.func->expr);
    if(l){
      add_gfunc_ref(l,m);
    }else l=m;
  }else if(ex->type==GFUNC){
    gfunc_table *gf;
    if((gf=name_to_gfunc(ex->data.gfunc->name))!=NULL){ /* really is a grid function */
      m=new_gfunc_ref();
      m->gfunc=gf;
      m->toff=ex->data.gfunc->toff;
      if(l)
        add_gfunc_ref(l,m);
      else l=m;
    }
  }
  return l;
}

gfunc_ref_list *find_gfuncs_if(const ifstat *ifst)
{
  gfunc_ref_list *dl,*dr,*dm,*dn;
  
  if(!ifst)
    return NULL;
    
  dl=find_gfuncs(ifst->lexpr);
  dr=find_gfuncs(ifst->expr);
  if(dl){
    add_gfunc_ref(dl,dr);
  }else dl=dr;
  if(ifst->etype==1){
    dr=find_gfuncs(ifst->elst.expr);
    if(dl){
      add_gfunc_ref(dl,dr);
    }else dl=dr;
  }else if(ifst->etype==2){
    dr=find_gfuncs_if(ifst->elst.ifst);
    if(dl){
      add_gfunc_ref(dl,dr);
    }else dl=dr;
  }
  return dl;
}

coord_list *find_coords(const node *ex)
{
  coord_list *l,*r,*m,*n;
  
  if(!ex)
    return NULL;
  
  l=find_coords(ex->left);
  r=find_coords(ex->right);
  
  if(l){
    add_name_ref(l,r);
  }else l=r;
  
  if(ex->type==FUNC){
    m=find_coords(ex->data.func->expr);
    if(l){
      add_name_ref(l,m);
    }else l=m;
  }else if(ex->type==COORD){
    m=new_coord_list();
    m->name=ex->data.coord->name;
    if(l)
      add_name_ref(l,m);
    else l=m;
  }
  return l;
}

coord_list *find_coords_if(const ifstat *ifst)
{
  coord_list *dl,*dr,*dm,*dn;
  
  if(!ifst)
    return NULL;
    
  dl=find_coords(ifst->lexpr);
  dr=find_coords(ifst->expr);
  if(dl){
    add_name_ref(dl,dr);
  }else dl=dr;
  if(ifst->etype==1){
    dr=find_coords(ifst->elst.expr);
    if(dl){
      add_name_ref(dl,dr);
    }else dl=dr;
  }else if(ifst->etype==2){
    dr=find_coords_if(ifst->elst.ifst);
    if(dl){
      add_name_ref(dl,dr);
    }else dl=dr;
  }
  return dl;
}

coord_list *find_cdifs(const node *ex)
{
  coord_list *l,*r,*m,*n;
  
  if(!ex)
    return NULL;
  
  l=find_cdifs(ex->left);
  r=find_cdifs(ex->right);
  
  if(l){
    add_name_ref(l,r);
  }else l=r;
  
  if(ex->type==FUNC){
    m=find_cdifs(ex->data.func->expr);
    if(l){
      add_name_ref(l,m);
    }else l=m;
  }else if(ex->type==IDEN){
    int i,j,d;
    for(i=0,d=0;i<ncoords && !d;i++)
      for(j=0;j<coords[i].rank && !d;j++)
        d=(ex->data.name == (*(coord_difs[i]+j)));
    if(d!=0){
      m=new_coord_list();
      m->name=ex->data.name;
      if(l)
        add_name_ref(l,m);
      else l=m;
    }
  }
  return l;
}

coord_list *find_cdifs_if(const ifstat *ifst)
{
  coord_list *dl,*dr,*dm,*dn;
  
  if(!ifst)
    return NULL;
    
  dl=find_cdifs(ifst->lexpr);
  dr=find_cdifs(ifst->expr);
  if(dl){
    add_name_ref(dl,dr);
  }else dl=dr;
  if(ifst->etype==1){
    dr=find_cdifs(ifst->elst.expr);
    if(dl){
      add_name_ref(dl,dr);
    }else dl=dr;
  }else if(ifst->etype==2){
    dr=find_cdifs_if(ifst->elst.ifst);
    if(dl){
      add_name_ref(dl,dr);
    }else dl=dr;
  }
  return dl;
}

param_ref_list *find_params(const node *ex)
{
  param_ref_list *l,*r,*m,*n;
  
  if(!ex)
    return NULL;
  
  l=find_params(ex->left);
  r=find_params(ex->right);
  
  if(l){
    add_param_ref(l,r);
  }else l=r;
  
  if(ex->type==FUNC){
    m=find_params(ex->data.func->expr);
    if(l){
      add_param_ref(l,m);
    }else l=m;
  }else if(ex->type==IDEN){
    if(name_type(ex->data.name)==PARAM){
      int i;
      m=new_param_ref();
      for(i=0;i<nparams && 
          params[i].name!=ex->data.name;i++);
      if(i==nparams)
        fatal_error("find_params: Funky goings on.");
      m->par=&params[i];
      if(l)
        add_param_ref(l,m);
      else l=m;
    }
  }
  return l;
}

param_ref_list *find_params_if(const ifstat *ifst)
{
  param_ref_list *dl,*dr,*dm,*dn;
  
  if(!ifst)
    return NULL;
    
  dl=find_params(ifst->lexpr);
  dr=find_params(ifst->expr);
  if(dl){
    add_param_ref(dl,dr);
  }else dl=dr;
  if(ifst->etype==1){
    dr=find_params(ifst->elst.expr);
    if(dl){
      add_param_ref(dl,dr);
    }else dl=dr;
  }else if(ifst->etype==2){
    dr=find_params_if(ifst->elst.ifst);
    if(dl){
      add_param_ref(dl,dr);
    }else dl=dr;
  }
  return dl;
}

qheader *init_queue()
{
  qheader *q;
  
  if((q=(qheader *)malloc(sizeof(qheader)))==NULL)
    fatal_error("Can't malloc qheader.");
  q->head=q->tail=NULL;
  return q;
}

void enqueue(qheader *qh, node *expr)
{
  qnode *qn;
  
  if(expr){
    if((qn=(qnode *)malloc(sizeof(qnode)))==NULL)
      fatal_error("Can't malloc qnode");
    qn->expr=expr;
    qn->next=NULL;
    if(qh->head){
      qh->tail->next=qn;
      qh->tail=qn;
    }else{
      qh->head=qh->tail=qn;
    }
  }
}

node *dequeue(qheader *qh)
{
  node *n;

  if(qh->head){
    n=qh->head->expr;
    qh->head=qh->head->next;
  }else n=NULL;
  return(n);
}

int gflev_num(const int gfnum, const int toff)
{
  int i,n;
  offset_type *of;
  
   /* check if time level exists */
  for(i=0,of=gfuncs[gfnum].tlev;of!=NULL && of->offset!=toff;of=of->next,i++);
  if(of==NULL){
    i=-1;
  }else{ /* yes, so count grid function levels */
      for(n=0;n<gfnum;n++)
         i+=gfuncs[n].ntlevs;
   }
  return i; 
}

int gflev(const int gfnum)
{
  int i,n;
  
   for(i=0,n=0;n<gfnum;n++)
      i+=gfuncs[n].ntlevs;
  return i; 
}

int get_size_one_n(const int len)
{
  int i,n,rlen;
  
   if(len>nresids) rlen=nresids; else rlen=len;
  for(i=0,n=0;i<rlen;){
    if(resids[i].eval)
      n++;
    for(++i;i<rlen && (resids[i].gfunc == resids[i-1].gfunc);i++);
  }
  return ngfuncs+n;
}

int get_size_one()
{
  int i,n;
  
  for(i=0,n=0;i<nresids;){
    if(resids[i].eval)
      n++;
    for(++i;i<nresids && (resids[i].gfunc == resids[i-1].gfunc);i++);
  }
  return ngfuncs+n;
}

int get_size_all_n(const int len)
{
  int i,n,rlen;

   if(len>nresids) rlen=nresids; else rlen=len;
  for(n=0,i=0;i<ngfuncs;i++)
    n+=gfuncs[i].ntlevs;
  for(i=0;i<rlen;){
    if(resids[i].eval)
      n++;
    for(++i;i<rlen && (resids[i].gfunc == resids[i-1].gfunc);i++);
  }
  return n;
}

int get_size_all()
{
  int i,n=0;

  for(i=0;i<ngfuncs;i++)
    n+=gfuncs[i].ntlevs;
  for(i=0;i<nresids;){
    if(resids[i].eval)
      n++;
    for(++i;i<nresids && (resids[i].gfunc == resids[i-1].gfunc);i++);
  }
  return n;
}

/* return index into grids given a pointer to a grids element */
int gptr_to_index(const grid_table *gr)
{
  int i;

  if(gr>=grids)
    i=(int)(gr-grids);
  else fatal_error("gptr_to_index: Pointer out of range.");
  return i;  
}

int update_to_index(const update_table *up)
{
  int i;

  if(up>=updates)
    i=(int)(up-updates);
  else fatal_error("update_to_index: pointer out of range.");
  return i;
}

int initer_to_index(const update_table *up)
{
  int i;

  if(up>=initers)
    i=(int)(up-initers);
  else fatal_error("initer_to_index: pointer out of range.");
  return i;
}

int gfunc_to_index(const gfunc_table *gf)
{
  int i;

  if(gf >= gfuncs)
    i=(int)(gf-gfuncs);
  else fatal_error("gfunc_to_index: pointer out of range.");
  return i;
}

int offset_to_index(const int tof, offset_type *ofl)
{
  int i;
  offset_type *of;
  
  for(i=0,of=ofl;of!=NULL && of->offset!=tof;of=of->next,i++);
  if(of==NULL){
    fprintf(stderr,"offset_to_index: offset %d doesn't exist\n",tof);
    i=0;
  }
  return i; 
}

//Moved above first call of coordsys_to_index by LR - needed for modern compilers//
//int coordsys_to_index(const coord_table *cd)
//{
//   int i;
//   
//   if(cd >= coords)
//      i=(int)(cd-coords);
//   else fatal_error("coordsys_to_index: pointer out of range.");
//   return i;
//}
//////////////////////////////

int gbase_to_gindex(name_list *nm)
{
   int i,j,done=0;
   i_reg *ir;
   
   for(i=0;!done && i<ngrids;i++){
      for(ir=grids[i].reg.bounds,j=0;!done && j<grids[i].reg.rank;j++,ir=ir->next){
         if((ir->upper->type==IDEN && ir->upper->data.name==nm) ||
            (ir->upper->type==OP && ir->upper->left!=NULL &&
             ir->upper->left->type==IDEN && ir->upper->left->data.name==nm)){
            done=1;
         }
      }
   }
   if(done)
      i--;
   else if(!done)
      i=-1;
   return i;
}

int check_gen_expr(node *ex)
{
  if(ex){
    if(ex->type == OP){
      if(ex->data.op=='e' || ex->data.op=='<' || ex->data.op=='>' ||
          ex->data.op=='l' || ex->data.op=='g' || ex->data.op=='n' ||
          ex->data.op=='a' || ex->data.op=='o'){
        fprintf(stderr,"check_gen_expr: expression contains illegal operator %c.\n",ex->data.op);
        return(0);
      }
    }
    if(check_gen_expr(ex->left))
      return check_gen_expr(ex->right);
    else return(0);
  }else return(1);
}

int check_gen_ifstat(ifstat *ifst)
{
  int done=0,res=1;
  ifstat *is;
  
  is=ifst;
  do{
    res*=check_gen_expr(is->expr);
    if(is->etype==0){
      done=1;
    }else if(is->etype==1){
      res*=check_gen_expr(is->elst.expr);
      done=1;
    }else if(is->etype==2){
      is=is->elst.ifst;
    }
  }while(!done);
  return res;
}

int check_simp_expr(node *ex)
{
  if(ex){
    if(ex->type == D_OP){
      fprintf(stderr,"check_simp_expr: expression contains a derivative operator.\n");
      return(0);
    }else if(ex->type == GFUNC){
      fprintf(stderr,"check_simp_expr: expression contains a grid function.\n");
      return(0);
    }else if(ex->type == COORD){
      fprintf(stderr,"check_simp_expr: expression contains a coordinate.\n");
      return(0);
    }else if(ex->type == FUNC){
      fprintf(stderr,"check_simp_expr: expression contains a function.\n");
      return(0);
    }
    if(check_gen_expr(ex->left))
      return check_gen_expr(ex->right);
    else return(0);
  }else return(0);
}

int check_log_expr(node *ex)
{
  return 1;
}

void resid_header(FILE *fp, const res_table *rs)
{
  name_list *cn;
  gfunc_ref_list *gl;
  coord_list *cl;
  param_ref_list *pr;
  char c;
  i_reg *ir;
  int i,j;
  
  if(language==C || language==IDF){
    cn=rs->gfunc->fname;
    fprintf(fp,"void res_%s(",cn->n);
    switch(rs->gfunc->type){
      case INT   : fprintf(fp,"int *%s_res",cn->n);
                   break;
      case FLOAT : fprintf(fp,"double *%s_res",cn->n);
                   break;
    }
    for(gl=rs->glob_gf;gl!=NULL;gl=gl->next){
      fprintf(fp,",");
      switch(gl->gfunc->type){
        case INT   : fprintf(fp,"int *");
                     break;
        case FLOAT : fprintf(fp,"double *");
                     break;
      }
      fprintf(fp,"%s",gl->gfunc->fname->n);
      gfunc_suffix(fp,gl->toff);
      if((gl->next && gl->next->gfunc->grd != gl->gfunc->grd) || !gl->next){
        for(i=0,cl=gl->gfunc->grd->clst;i<gl->gfunc->grd->reg.rank;i++,cl=cl->next){
          fprintf(fp,",int %s_N%s",gl->gfunc->grd->name->n,cl->name->n);
        }
      }
    }
    for(cl=rs->glob_crds;cl!=NULL;cl=cl->next){
      if(is_space(cl->name))
        fprintf(fp,",double *%s",cl->name->n);
      else fprintf(fp,",double %s",cl->name->n);
    }
    for(cl=rs->glob_cdifs;cl!=NULL;cl=cl->next){
      fprintf(fp,",double %s",cl->name->n);
    }
    for(pr=rs->glob_par;pr!=NULL;pr=pr->next){
      switch(pr->par->type){
        case  INT    :  fprintf(fp,",int ");
                      break;
        case  FLOAT  :  fprintf(fp,",double ");
                      break;
      }
      if(vsize(pr->par->size)>1)
        fprintf(fp,"*");
      fprintf(fp,"%s",pr->par->name->n);
    }
    fprintf(fp,")\n");
    fprintf(fp,"{\n");
    for(c='i',ir=rs->reg;ir!=NULL;ir=ir->next,c++)
      fprintf(fp,"  int %c;\n",c);
    fprintf(fp,"\n");
  }else if(language==ALLF || language==F77 || language==F90 || language==UPF){
    cn=rs->gfunc->fname;
    sprintf(forbuf,"      subroutine res_%s(%s_res",cn->n,cn->n);
    fort_out(fp,forbuf);
    for(gl=rs->glob_gf;gl!=NULL;gl=gl->next){
      fort_out(fp,",");
      fort_out(fp,gl->gfunc->fname->n);
      gfunc_suffix_f(fp,gl->toff);
      if((gl->next && gl->next->gfunc->grd != gl->gfunc->grd) || !gl->next){
        for(i=0,cl=gl->gfunc->grd->clst;i<gl->gfunc->grd->reg.rank;i++,cl=cl->next){
          fort_out(fp,",");
          sprintf(forbuf,"%s_N%s",gl->gfunc->grd->name->n,cl->name->n);
          fort_out(fp,forbuf);
        }
      }
    }
    for(cl=rs->glob_crds;cl!=NULL;cl=cl->next){
      fort_out(fp,",");
      i=sprintf(forbuf,"%s",cl->name->n);
      fort_out(fp,forbuf);
    }
    for(cl=rs->glob_cdifs;cl!=NULL;cl=cl->next){
      fort_out(fp,",");
      i=sprintf(forbuf,"%s",cl->name->n);
      fort_out(fp,forbuf);
    }
    for(pr=rs->glob_par;pr!=NULL;pr=pr->next){
      fort_out(fp,",");
      fort_out(fp,pr->par->name->n);
    }
    fort_out(fp,")\n");
    fort_out(fp,"        implicit none\n\n");
      if(language==ALLF)
       fort_out(fp,"        include 'globals.inc'\n\n");

    if(gl=rs->glob_gf){
      fort_out(fp,"        integer ");
      for(i=0,cl=gl->gfunc->grd->clst;i<gl->gfunc->grd->reg.rank;i++,cl=cl->next){
        sprintf(forbuf,"%s_N%s",gl->gfunc->grd->name->n,cl->name->n);
        fort_out(fp,forbuf);
        if(i<gl->gfunc->grd->reg.rank-1)
          fort_out(fp,",");
        else fort_out(fp,"\n");
      }
    }

    sprintf(forbuf,"        real*8  %s_res",cn->n);
    fort_out(fp,forbuf);
    array_bounds_to_string(fp,rs->gfunc->grd,0);
    fort_out(fp,"\n");

    for(;gl!=NULL;gl=gl->next){
      switch(gl->gfunc->type){
        case INT   : fort_out(fp,"        integer ");
                     break;
        case FLOAT : fort_out(fp,"        real*8  ");
                     break;
      }
      if(!gl->name){
        fort_out(fp,gl->gfunc->fname->n);
        gfunc_suffix_f(fp,gl->toff);
      }else{
        fort_out(fp,gl->name->n);
      }
      array_bounds_to_string(fp,gl->gfunc->grd,0);
      fort_out(fp,"\n");
      if((gl->next && gl->next->gfunc->grd != gl->gfunc->grd)){
        fort_out(fp,"        integer ");
        for(i=0,cl=gl->next->gfunc->grd->clst;i<gl->next->gfunc->grd->reg.rank;i++,cl=cl->next){
          sprintf(forbuf,"%s_N%s",gl->next->gfunc->grd->name->n,cl->name->n);
          fort_out(fp,forbuf);
          if(i<gl->next->gfunc->grd->reg.rank-1)
            fort_out(fp,",");
          else fort_out(fp,"\n");
        }
      }
    }

    for(cl=rs->glob_crds;cl!=NULL;cl=cl->next){
      if(is_space(cl->name)){
        sprintf(forbuf,"        real*8  %s(*)\n",cl->name->n);
        fort_out(fp,forbuf);
      }else{
        sprintf(forbuf,"        real*8  %s\n",cl->name->n);
        fort_out(fp,forbuf);
      }
    }
    for(cl=rs->glob_cdifs;cl!=NULL;cl=cl->next){
      sprintf(forbuf,"        real*8  %s\n",cl->name->n);
      fort_out(fp,forbuf);
    }
    for(pr=rs->glob_par;pr!=NULL;pr=pr->next){
      switch(pr->par->type){
        case  INT    :  fort_out(fp,"        integer ");
                      break;
        case  FLOAT  :  fort_out(fp,"        real*8  ");
                      break;
      }
      fort_out(fp,pr->par->name->n);
      if(vsize(pr->par->size)>1){
        fort_out(fp,"(");
        for(j=0;j<pr->par->size->dim;j++){
          sprintf(forbuf,"%d",pr->par->size->size[j]);
          fort_out(fp,forbuf);
          if(j<pr->par->size->dim-1)
            fort_out(fp,",");
        }
        fort_out(fp,")");
      }
      fort_out(fp,"\n");
    }
    for(c='i',ir=rs->reg;ir!=NULL;ir=ir->next,c++){
      sprintf(forbuf,"        integer %c\n",c);
      fort_out(fp,forbuf);
    }
    /*
    for(j=0;j<ncoords;j++)
       for(i=0;i<coords[j].rank-1;i++){
          sprintf(forbuf,"        integer %s\n",grid_base[j][i]->n);
          fort_out(fp,forbuf);
       }    
    fort_out(fp,"\n");
    for(j=0;j<ncoords;j++)
      for(i=0;i<coords[j].rank-1;i++){
        sprintf(forbuf,"        %s = %s0 * 2**level + 1\n",grid_base[j][i]->n,
                grid_base[j][i]->n);
        fort_out(fp,forbuf);
      }
    */
  }
}

void resid_call(FILE *fp, const res_table *rs)
{
  name_list *cn;
  gfunc_ref_list *gl;
  coord_list *cl;
  param_ref_list *pr;
  char c;
  i_reg *ir;
  int j,i,ev;
  
  if(language==C || language==IDF){
    cn=rs->gfunc->fname;
    fprintf(fp,"res_%s(%s_res",cn->n,cn->n);
    for(gl=rs->glob_gf;gl!=NULL;gl=gl->next){
      fprintf(fp,",");
      fprintf(fp,"%s",gl->gfunc->fname->n);
      gfunc_suffix(fp,gl->toff);
      if((gl->next && gl->next->gfunc->grd != gl->gfunc->grd) || !gl->next){
        for(i=0,cl=gl->gfunc->grd->clst;i<gl->gfunc->grd->reg.rank;i++,cl=cl->next){
          fprintf(fp,",%s_N%s",gl->gfunc->grd->name->n,cl->name->n);
        }
      }
    }
    for(cl=rs->glob_crds;cl!=NULL;cl=cl->next){
      fprintf(fp,",%s",cl->name->n);
    }
    for(cl=rs->glob_cdifs;cl!=NULL;cl=cl->next){
      fprintf(fp,",%s",cl->name->n);
    }
    for(pr=rs->glob_par;pr!=NULL;pr=pr->next){
      fprintf(fp,",%s",pr->par->name->n);
    }
    fprintf(fp,");\n");
  }else if(language==F77 || language==F90 || language==UPF){
     char nm[256];
    cn=rs->gfunc->fname;
    sprintf(nm,"res_%s",cn->n);
    fort_call(nm);
    fprintf(fp,"%s(%s_res",nm,cn->n);
    for(gl=rs->glob_gf;gl!=NULL;gl=gl->next){
      fprintf(fp,",");
      fprintf(fp,"%s",gl->gfunc->fname->n);
      gfunc_suffix(fp,gl->toff);
      if((gl->next && gl->next->gfunc->grd != gl->gfunc->grd) || !gl->next){
        for(i=0,cl=gl->gfunc->grd->clst;i<gl->gfunc->grd->reg.rank;i++,cl=cl->next)
          fprintf(fp,",&%s_N%s",gl->gfunc->grd->name->n,cl->name->n);
      }
    }
    for(cl=rs->glob_crds;cl!=NULL;cl=cl->next){
      if(is_space(cl->name))
        fprintf(fp,",%s",cl->name->n);
      else fprintf(fp,",&%s",cl->name->n);
    }
    for(cl=rs->glob_cdifs;cl!=NULL;cl=cl->next){
      fprintf(fp,",&%s",cl->name->n);
    }
    for(pr=rs->glob_par;pr!=NULL;pr=pr->next){
       if(vsize(pr->par->size)==1)
          fprintf(fp,",&%s",pr->par->name->n);
       else fprintf(fp,",%s",pr->par->name->n);
    }
    fprintf(fp,");\n");
  }else if(language==ALLF){
    cn=rs->gfunc->fname;
    j=resid_exists(cn);
    for(ev=0,i=0;i<j;){
      if(resids[i].eval)
        ev++;
      for(i++;i<nresids && resids[i].gfunc==resids[i-1].gfunc;i++);
    }
    sprintf(forbuf,"call res_%s(q(ptrs(gf_st(%d)+1))",cn->n,ngfuncs+ev+1);
    fort_out(fp,forbuf);
    for(gl=rs->glob_gf;gl!=NULL;gl=gl->next){
      fort_out(fp,",");
      sprintf(forbuf,"q(ptrs(gf_st(%d)+%d))",gfunc_to_index(gl->gfunc)+1,
              offset_to_index(gl->toff,gl->gfunc->tlev)+1);
      fort_out(fp,forbuf);
      if((gl->next && gl->next->gfunc->grd != gl->gfunc->grd) || !gl->next){
        for(i=0;i<gl->gfunc->grd->reg.rank;i++){
          fort_out(fp,",");
          sprintf(forbuf,"ptrs(getpshape(%d)+%d)",gptr_to_index(gl->gfunc->grd)+1,i);
          fort_out(fp,forbuf);
        }
      }
    }
    if(rs->glob_crds){
      fort_out(fp,",");
      for(cl=rs->glob_crds;cl!=NULL;cl=cl->next){
        if(is_space(cl->name))
          sprintf(forbuf,"q(%s)",cl->name->n);
        else sprintf(forbuf,"%s",cl->name->n);
        fort_out(fp,forbuf);
        if(cl->next)
          fort_out(fp,",");
      }
    }
    if(rs->glob_cdifs){
      fort_out(fp,",");
      for(cl=rs->glob_cdifs;cl!=NULL;cl=cl->next){
        fort_out(fp,cl->name->n);
        if(cl->next)
          fort_out(fp,",");
      }
    }
    if(rs->glob_par){
      fort_out(fp,",");
      for(pr=rs->glob_par;pr!=NULL;pr=pr->next){
        fort_out(fp,pr->par->name->n);
        if(pr->next)
          fort_out(fp,",");
      }
    }
    fort_out(fp,")\n");
  }
}

void update_header(FILE *fp, const update_table *up)
{
  gfunc_ref_list *gl;
  coord_list *cl;
  i_reg *ir;
  int i,j,fl;
  gfunc_tab_list *gt;
  work_list *wl;
  param_ref_list *pr;
  
  fl=!mystrcmp("auto",up->type->n);
  if(language==C || language==IDF){
    fprintf(fp,"/*  This routine updates the following grid functions \n");
    fprintf(fp,"    ");
    for(gt=up->gfs;gt!=NULL;gt=gt->next)
      fprintf(fp,"%s ",gt->gfunc->fname->n);
    fprintf(fp,"\n*/\n");
    if(up->name)
      fprintf(fp,"void %s(int *rnpldone,",up->name->n);
    else
      fprintf(fp,"void update%d(int *rnpldone,",update_to_index(up));
    for(gl=up->glob_gf;gl!=NULL;gl=gl->next){
      switch(gl->gfunc->type){
        case INT   : fprintf(fp,"int *");
                     break;
        case FLOAT : fprintf(fp,"double *");
                     break;
      }
      if(!gl->name){
        fprintf(fp,"%s",gl->gfunc->fname->n);
        gfunc_suffix(fp,gl->toff);
      }else fprintf(fp,"%s",gl->name->n);
      if((gl->next && gl->next->gfunc->grd != gl->gfunc->grd) || !gl->next){
        for(i=0,cl=gl->gfunc->grd->clst;i<gl->gfunc->grd->reg.rank;i++,cl=cl->next){
          fprintf(fp,",int %s_N%s",gl->gfunc->grd->name->n,cl->name->n);
        }
      }
      if(gl->next) fprintf(fp,",");
    }
    for(cl=up->glob_crds;cl!=NULL;cl=cl->next){
      if(is_space(cl->name))
        fprintf(fp,",double *%s",cl->name->n);
      else fprintf(fp,",double %s",cl->name->n);
    }
    for(cl=up->glob_cdifs;cl!=NULL;cl=cl->next){
      fprintf(fp,",double %s",cl->name->n);
    }
    for(pr=up->glob_par;pr!=NULL;pr=pr->next){
      switch(pr->par->type){
        case  INT    :  fprintf(fp,",int ");
                      break;
        case  FLOAT  :  fprintf(fp,",double ");
                      break;
      }
      if(vsize(pr->par->size)>1)
        fprintf(fp,"*");
      fprintf(fp,"%s",pr->par->name->n);
    }
    for(wl=up->work_refs;wl!=NULL;wl=wl->next)
      fprintf(fp,",double *work%d, int nwork%d",wl->work->num,wl->work->num);
    fprintf(fp,")\n");
    fprintf(fp,"{\n");
    if(fl)
      fprintf(fp,"  int i,j,k;\n");
    fprintf(fp,"\n");
  }else if(language==F77 || language==F90 || language==ALLF || language==UPF){
    int ar=0;
    fort_out(fp,"!----------------------------------------------------------------------\n");
    fort_out(fp,"!  This routine updates the following grid functions\n");
    fort_out(fp,"!  ");
    for(gt=up->gfs;gt!=NULL;gt=gt->next){
      sprintf(forbuf,"%s ",gt->gfunc->fname->n);
      fortcom_out(fp,forbuf);
    }
    fort_out(fp,"\n");
    fort_out(fp,"!----------------------------------------------------------------------\n");
    if(language==ALLF){
       if(up->name)
         sprintf(forbuf,"      subroutine %s(",up->name->n);
       else
         sprintf(forbuf,"      subroutine update%d(",update_to_index(up));
    }else{
       if(up->name)
         sprintf(forbuf,"      subroutine %s(rnpldone,",up->name->n);
       else
         sprintf(forbuf,"      subroutine update%d(rnpldone,",update_to_index(up));
    }
    fort_out(fp,forbuf);
    for(gl=up->glob_gf;gl!=NULL;gl=gl->next){
      if(!gl->name){
        fort_out(fp,gl->gfunc->fname->n);
        gfunc_suffix_f(fp,gl->toff);
      }else{
        fort_out(fp,gl->name->n);
        if(gl->array)
          while(gl->next && gl->gfunc==gl->next->gfunc)
            gl=gl->next;
      }
      if((gl->next && gl->next->gfunc->grd != gl->gfunc->grd) || !gl->next){
        for(i=0,cl=gl->gfunc->grd->clst;i<gl->gfunc->grd->reg.rank;i++,cl=cl->next){
          fort_out(fp,",");
          sprintf(forbuf,"%s_N%s",gl->gfunc->grd->name->n,cl->name->n);
          fort_out(fp,forbuf);
        }
      }
      if(gl->next) fort_out(fp,",");
    }
    if(up->glob_crds){
      fort_out(fp,",");
      for(cl=up->glob_crds;cl!=NULL;cl=cl->next){
        fort_out(fp,cl->name->n);
        if(cl->next)
          fort_out(fp,",");
      }
    }
    if(up->glob_cdifs){
      fort_out(fp,",");
      for(cl=up->glob_cdifs;cl!=NULL;cl=cl->next){
        fort_out(fp,cl->name->n);
        if(cl->next)
          fort_out(fp,",");
      }
    }
    if(up->glob_par){
      fort_out(fp,",");
      for(pr=up->glob_par;pr!=NULL;pr=pr->next){
        fort_out(fp,pr->par->name->n);
        if(pr->next)
          fort_out(fp,",");
      }
    }
    if(up->work_refs){
      fort_out(fp,",");    
      for(wl=up->work_refs;wl!=NULL;wl=wl->next){
        sprintf(forbuf,"work%d,",wl->work->num);
        fort_out(fp,forbuf);
        sprintf(forbuf,"nwork%d",wl->work->num);
        fort_out(fp,forbuf);
        if(wl->next!=NULL)
          fort_out(fp,",");
      }
    }  
    fort_out(fp,")\n");
    fort_out(fp,"        implicit none\n\n");
    if(language==ALLF)
       fort_out(fp,"        include 'globals.inc'\n\n");
    else
       fort_out(fp,"        integer rnpldone\n");
    if(gl=up->glob_gf){
      fort_out(fp,"        integer ");
      for(i=0,cl=gl->gfunc->grd->clst;i<gl->gfunc->grd->reg.rank;i++,cl=cl->next){
        sprintf(forbuf,"%s_N%s",gl->gfunc->grd->name->n,cl->name->n);
        fort_out(fp,forbuf);
        if(i<gl->gfunc->grd->reg.rank-1)
          fort_out(fp,",");
        else fort_out(fp,"\n");
      }
    }
    for(;gl!=NULL;gl=gl->next){
      switch(gl->gfunc->type){
        case INT    : fort_out(fp,"        integer ");
                      break;
        case FLOAT  : fort_out(fp,"        real*8  ");
                      break;
        case COMPLEX:  fort_out(fp,"        complex ");
                      break;
      }
      if(!gl->name){
        fort_out(fp,gl->gfunc->fname->n);
        gfunc_suffix_f(fp,gl->toff);
      }else{
        fort_out(fp,gl->name->n);
        if(gl->array){
          while(gl->next && gl->gfunc==gl->next->gfunc)
            gl=gl->next;
          ar=1;
        }
      }
      array_bounds_to_string(fp,gl->gfunc->grd,ar);
      if(ar){
        sprintf(forbuf,",%d)",gl->gfunc->ntlevs);
        fort_out(fp,forbuf);
      }
      ar=0;
      fort_out(fp,"\n");
      if((gl->next && gl->next->gfunc->grd != gl->gfunc->grd)){
        fort_out(fp,"        integer ");
        for(i=0,cl=gl->next->gfunc->grd->clst;i<gl->next->gfunc->grd->reg.rank;i++,cl=cl->next){
          sprintf(forbuf,"%s_N%s",gl->next->gfunc->grd->name->n,cl->name->n);
          fort_out(fp,forbuf);
          if(i<gl->next->gfunc->grd->reg.rank-1)
            fort_out(fp,",");
          else fort_out(fp,"\n");
        }
      }
    }
    for(cl=up->glob_crds;cl!=NULL;cl=cl->next){
      if(is_space(cl->name)){
        sprintf(forbuf,"        real*8  %s(*)\n",cl->name->n);
        fort_out(fp,forbuf);
      }else{
        sprintf(forbuf,"        real*8  %s\n",cl->name->n);
        fort_out(fp,forbuf);
      }
    }
    for(cl=up->glob_cdifs;cl!=NULL;cl=cl->next){
      sprintf(forbuf,"        real*8  %s\n",cl->name->n);
      fort_out(fp,forbuf);
    }
    for(pr=up->glob_par;pr!=NULL;pr=pr->next){
      switch(pr->par->type){
        case  INT    :  fort_out(fp,"        integer ");
                      break;
        case  FLOAT  :  fort_out(fp,"        real*8  ");
                      break;
      }
      fort_out(fp,pr->par->name->n);
      if(vsize(pr->par->size)>1){
        fort_out(fp,"(");
        for(j=0;j<pr->par->size->dim;j++){
          sprintf(forbuf,"%d",pr->par->size->size[j]);
          fort_out(fp,forbuf);
          if(j<pr->par->size->dim-1)
            fort_out(fp,",");
        }
        fort_out(fp,")");
      }
      fort_out(fp,"\n");
    }
    for(wl=up->work_refs;wl!=NULL;wl=wl->next){
      sprintf(forbuf,"        integer nwork%d\n",wl->work->num);
      fort_out(fp,forbuf);
      sprintf(forbuf,"        real*8  work%d(nwork%d)\n",wl->work->num,
              wl->work->num);
      fort_out(fp,forbuf);
    }
    if(fl){
      fort_out(fp,"        integer i,j,k\n");
      /*
      for(j=0;j<ncoords;j++)
         for(i=0;i<coords[j].rank-1;i++){
            sprintf(forbuf,"        integer %s\n",grid_base[j][i]->n);
            fort_out(fp,forbuf);
         }    
      fort_out(fp,"\n");
      for(j=0;j<ncoords;j++)
        for(i=0;i<coords[j].rank-1;i++){
          sprintf(forbuf,"        %s = %s0 * 2**level + 1\n",grid_base[j][i]->n,
                  grid_base[j][i]->n);
          fort_out(fp,forbuf);
        }
      */
    }
    fort_out(fp,"\n");
  }
}

/* output function call to update up.  Caller must indent */
void update_call(FILE *fp, const update_table *up)
{
  gfunc_ref_list *gl;
  coord_list *cl;
  i_reg *ir;
  int i,j;
  work_list *wl;
  param_ref_list *pr;
  
  if(language==C || language==IDF){
    if(up->name)
      fprintf(fp,"%s(rnpldone,",up->name->n);
    else
      fprintf(fp,"update%d(rnpldone,",update_to_index(up));
    for(gl=up->glob_gf;gl!=NULL;gl=gl->next){
      fprintf(fp,"%s",gl->gfunc->fname->n);
      gfunc_suffix(fp,gl->toff);
      if((gl->next && gl->next->gfunc->grd != gl->gfunc->grd) || !gl->next){
        for(i=0,cl=gl->gfunc->grd->clst;i<gl->gfunc->grd->reg.rank;i++,cl=cl->next){
          fprintf(fp,",%s_N%s",gl->gfunc->grd->name->n,cl->name->n);
        }
      }
      if(gl->next) fprintf(fp,",");
    }
    for(cl=up->glob_crds;cl!=NULL;cl=cl->next){
      fprintf(fp,",%s",cl->name->n);
    }
    for(cl=up->glob_cdifs;cl!=NULL;cl=cl->next){
      fprintf(fp,",%s",cl->name->n);
    }
    for(pr=up->glob_par;pr!=NULL;pr=pr->next){
      fprintf(fp,",%s",pr->par->name->n);
    }
    for(wl=up->work_refs;wl!=NULL;wl=wl->next)
      fprintf(fp,",work%d,nwork%d",wl->work->num,wl->work->num);
    fprintf(fp,");\n");
  }else if(language==F77 || language==F90 || language==UPF){
     char nm[256];
    if(up->name){
       strcpy(nm,up->name->n);
    }else{
      sprintf(nm,"update%d",update_to_index(up));
    }
    fort_call(nm);
    fprintf(fp,"%s(rnpldone,",nm);
    for(gl=up->glob_gf;gl!=NULL;gl=gl->next){
      fprintf(fp,"%s",gl->gfunc->fname->n);
      gfunc_suffix(fp,gl->toff);
      if((gl->next && gl->next->gfunc->grd != gl->gfunc->grd) || !gl->next){
        for(i=0,cl=gl->gfunc->grd->clst;i<gl->gfunc->grd->reg.rank;i++,cl=cl->next)
          fprintf(fp,",&%s_N%s",gl->gfunc->grd->name->n,cl->name->n);
      }
      if(gl->next) fprintf(fp,",");
    }
    for(cl=up->glob_crds;cl!=NULL;cl=cl->next){
      if(is_space(cl->name))
        fprintf(fp,",%s",cl->name->n);
      else fprintf(fp,",&%s",cl->name->n);
    }
    for(cl=up->glob_cdifs;cl!=NULL;cl=cl->next){
      fprintf(fp,",&%s",cl->name->n);
    }
    for(pr=up->glob_par;pr!=NULL;pr=pr->next){
       if(vsize(pr->par->size)==1)
          fprintf(fp,",&%s",pr->par->name->n);
       else fprintf(fp,",%s",pr->par->name->n);
    }
    for(wl=up->work_refs;wl!=NULL;wl=wl->next)
      fprintf(fp,",work%d,&nwork%d",wl->work->num,wl->work->num);
    fprintf(fp,");\n");
  }else if(language==ALLF){
    if(up->name)
      sprintf(forbuf,"call %s(",up->name->n);
    else
      sprintf(forbuf,"call update%d(",update_to_index(up));
    fort_out(fp,forbuf);
    for(gl=up->glob_gf;gl!=NULL;gl=gl->next){
      sprintf(forbuf,"q(ptrs(gf_st(%d)+%d))",gfunc_to_index(gl->gfunc)+1,
              offset_to_index(gl->toff,gl->gfunc->tlev)+1);
      fort_out(fp,forbuf);
      if(gl->array)
        while(gl->next && gl->gfunc==gl->next->gfunc)
          gl=gl->next;
      if((gl->next && gl->next->gfunc->grd != gl->gfunc->grd) || !gl->next){
        for(i=0;i<gl->gfunc->grd->reg.rank;i++){
          fort_out(fp,",");
          sprintf(forbuf,"ptrs(getpshape(%d)+%d)",gptr_to_index(gl->gfunc->grd)+1,i);
          fort_out(fp,forbuf);
        }
      }
      if(gl->next) fort_out(fp,",");
    }
    if(up->glob_crds){
      fort_out(fp,",");
      for(cl=up->glob_crds;cl!=NULL;cl=cl->next){
        if(is_space(cl->name))
          sprintf(forbuf,"q(%s)",cl->name->n);
        else sprintf(forbuf,"%s",cl->name->n);
        fort_out(fp,forbuf);
        if(cl->next)
          fort_out(fp,",");
      }
    }
    if(up->glob_cdifs){
      fort_out(fp,",");
      for(cl=up->glob_cdifs;cl!=NULL;cl=cl->next){
        fort_out(fp,cl->name->n);
        if(cl->next)
          fort_out(fp,",");
      }
    }
    if(up->glob_par){
      fort_out(fp,",");
      for(pr=up->glob_par;pr!=NULL;pr=pr->next){
        fort_out(fp,pr->par->name->n);
        if(pr->next)
          fort_out(fp,",");
      }
    }
    if(up->work_refs){
      fort_out(fp,",");    
      for(wl=up->work_refs;wl!=NULL;wl=wl->next){
        sprintf(forbuf,"q(work%d),",wl->work->num);
        fort_out(fp,forbuf);
        sprintf(forbuf,"nwork%d",wl->work->num);
        fort_out(fp,forbuf);
        if(wl->next)
          fort_out(fp,",");
      }
    }  
    fort_out(fp,")\n");
  }
}

void output_resid_stat(FILE *fp, const res_table *rs)
{
  if(language==F77 || language==F90 || language==ALLF || language==UPF){
    if(rs->etype==1){
      sprintf(forbuf,"          %s_res",rs->gfunc->fname->n);
      fort_out(fp,forbuf);
      array_ref0_to_string_f(fp,rs->gfunc->grd);
      fort_out(fp,"="); 
      ex_to_string_f(fp,rs->elst.expr);
      fort_out(fp,"\n");
    }else{
      int done=0;
      ifstat *is;
      is=rs->elst.ifst;
      fort_out(fp,"         ");
      do{
        fort_out(fp," if(");
        ex_to_string_f(fp,is->lexpr);
        fort_out(fp,") then\n");
        sprintf(forbuf,"            %s_res",rs->gfunc->fname->n);
        fort_out(fp,forbuf);
        array_ref0_to_string_f(fp,rs->gfunc->grd);
        fort_out(fp,"="); 
        ex_to_string_f(fp,is->expr);
        fort_out(fp,"\n");
        if(is->etype==0){
          done=1;
        }else if(is->etype==1){
          fort_out(fp,"          else\n");
          sprintf(forbuf,"            %s_res",rs->gfunc->fname->n);
          fort_out(fp,forbuf);
          array_ref0_to_string_f(fp,rs->gfunc->grd);
          fort_out(fp,"="); 
          ex_to_string_f(fp,is->elst.expr);
          fort_out(fp,"\n");
          done=1;
        }else if(is->etype==2){
          fort_out(fp,"          else");
          is=is->elst.ifst;
        }
      }while(!done);
      fort_out(fp,"          end if\n");
    }
  }else if(language==C || language==IDF){
    if(rs->etype==1){
      fprintf(fp,"    *(%s_res + ",rs->gfunc->fname->n);
      array_ref0_to_string(fp,rs->gfunc->grd);
      fprintf(fp,")="); 
      ex_to_string(fp,rs->elst.expr);
      fprintf(fp,";\n"); 
    }else{
      int done=0;
      ifstat *is;
      is=rs->elst.ifst;
      fprintf(fp,"   ");
      do{
        fprintf(fp," if(");
        ex_to_string(fp,is->lexpr);
        fprintf(fp,")\n");
        fprintf(fp,"      *(%s_res +",rs->gfunc->fname->n);
        array_ref0_to_string(fp,rs->gfunc->grd);
        fprintf(fp,")="); 
        ex_to_string(fp,is->expr);
        fprintf(fp,";\n");
        if(is->etype==0){
          done=1;
        }else if(is->etype==1){
          fprintf(fp,"    else\n");
          fprintf(fp,"      *(%s_res +",rs->gfunc->fname->n);
          array_ref0_to_string(fp,rs->gfunc->grd);
          fprintf(fp,")="); 
          ex_to_string(fp,is->elst.expr);
          fprintf(fp,";\n");
          done=1;
        }else if(is->etype==2){
          fprintf(fp,"    else");
          is=is->elst.ifst;
        }
      }while(!done);
    }
  }
}

void write_single_stat(FILE *fp, const res_table *rs, node *e)
{
  int j;
  gfunc_ref_list *grl;
  node *m,*d,*gf;
  index_list *o;

  m=new_node();
  d=new_node();
  gf=new_node();
  m->type=OP;
  m->data.op='-';
  d->type=OP;
  d->data.op='/';
  gf->type=GFUNC;
  gf->data.gfunc=new_gfunc();
  for(grl=rs->gf_refs;grl!=NULL && grl->gfunc!=rs->gfunc;grl=grl->next);
  if(grl==NULL){
    fprintf(stderr,"Grid function <%s> has a residual which doesn't reference itself.\n",
            rs->gfunc->fname->n);
    ex_to_string(stderr,e);
    fprintf(stderr,"\n");
    fatal_error("Specification error.");
  }
  gf->data.gfunc->toff=grl->toff; /* most advanced time level in residual */
  gf->data.gfunc->name=rs->gfunc->fname;
  gf->data.gfunc->grd=rs->gfunc->grd;
  gf->data.gfunc->indx=new_index_list();
  gf->data.gfunc->indx->ind=new_indel();
  for(o=gf->data.gfunc->indx,j=0;j<gf->data.gfunc->grd->reg.rank;j++,o=o->next){
    o->ind->type=OFFS;
    o->ind->val.offset=0;
    if(j+1<gf->data.gfunc->grd->reg.rank){
      o->next=new_index_list();
      o->next->ind=new_indel();
    }else o->next=NULL;
  }
  gf->left=gf->right=NULL;
  m->left=gf;
  m->right=d;
  d->left=e;
  d->right=deriv_wrt_gfunc(e,gf);
  if((d->right->type==NUM && d->right->data.num==0.0) || (d->right->type==INUM && d->right->data.inum==0)){
    fprintf(stderr,"Grid function <%s> has a residual which doesn't contain ",
            rs->gfunc->fname->n);
    ex_to_string(stderr,gf);
    fprintf(stderr,"\n");
    ex_to_string(stderr,e);
    fprintf(stderr,"\n");
    fatal_error("Specification error.");
  }
  simplify_expr(m);
  if(language==F77 || language==F90 || language==ALLF || language==UPF){
    fort_out(fp,"          ");
     ex_to_string_f(fp,gf);
    fort_out(fp,"=");
     ex_to_string_f(fp,m);
    fort_out(fp,"\n");
  }else if(language==C || language==IDF){
    fprintf(fp,"    ");
     ex_to_string(fp,gf);
    fprintf(fp,"=");
     ex_to_string(fp,m);
    fprintf(fp,";\n");
  }
}

void output_update_stat(FILE *fp, const res_table *rs)
{
  gfunc_ref_list *grl;
  
#ifdef RNPLDEBUG
fprintf(stderr,"output_update_stat: nresids=%d\n",nresids);
#endif
  if(rs->etype==1){
    write_single_stat(fp,rs,rs->elst.expr);
  }else{
    int done=0;
    ifstat *is;
    is=rs->elst.ifst;
    if(language==F77 || language==F90 || language==ALLF || language==UPF)
      fort_out(fp,"         ");
    else if(language==C || language==IDF)
      fprintf(fp,"   ");
    do{
      if(language==F77 || language==F90 || language==ALLF || language==UPF){
        fort_out(fp," if(");
        ex_to_string_f(fp,is->lexpr);
        fort_out(fp,") then\n");
        fort_out(fp,"  ");
        write_single_stat(fp,rs,is->expr);
        if(is->etype==0){
          fort_out(fp,"          end if\n");
          done=1;
        }else if(is->etype==1){
          fort_out(fp,"          else\n");
          fort_out(fp,"  ");
          write_single_stat(fp,rs,is->elst.expr);
          fort_out(fp,"          end if\n");
          done=1;
        }else if(rs->elst.ifst->etype==2){
          fort_out(fp,"          else");
          is=is->elst.ifst;
        }
      }else if(language==C || language==IDF){
        fprintf(fp," if(");
        ex_to_string(fp,is->lexpr);
        fprintf(fp,")\n  ");
        write_single_stat(fp,rs,is->expr);
        if(is->etype==0){
          done=1;
        }else if(is->etype==1){
          fprintf(fp,"    else\n  ");
          write_single_stat(fp,rs,is->elst.expr);
          done=1;
        }else if(rs->elst.ifst->etype==2){
          fprintf(fp,"    else");
          is=is->elst.ifst;
        }
      }
    }while(!done);
  }
}

void declare_coord_difs(FILE *fp)
{
  int i,j,res;
  coord_list *cl;
  work_list *wl;

   if(language!=ALLF){
      /* moved from c_header */
     fprintf(fp,"  int rmod;\n");
     for(j=0;j<ncoords;j++){
       for(res=0,i=0;i<j;i++)
         res+=(coord_difs[j][0] == coord_difs[i][0]);
       if(res==0) /* only declare unique time coordinates */
         fprintf(fp,"  double %s;\n",coord_difs[j][0]->n);
       for(cl=coords[j].c_names->next,i=1;i<coords[j].rank;i++,cl=cl->next){
         fprintf(fp,"  double %s;\n",coord_difs[j][i]->n);
         fprintf(fp,"  double *%s;\n",cl->name->n);
       }
     }
  }else{
    fprintf(fp,"!----------------------------------------------------------------------\n");
    fprintf(fp,"!  Parameters\n");
    fprintf(fp,"!----------------------------------------------------------------------\n");
    for(i=0;i<nparams;i++){
      if(params[i].con==0){
        switch(params[i].type){
          case IVEC    : if(params[i].def && (IVEL*params[i].def_val.i_ar[0] +1 > IVEC_SIZE)){
                          fprintf(stderr,"declare_coord_difs: WARNING: default IVEC size must be increased.\n");
                          fprintf(stderr,"  IVEC %s needs size of %d\n",params[i].name->n,
                                  IVEL*params[i].def_val.i_ar[0] +1);
                        }
          case INT    : fprintf(fp,"       integer       ");
                        break;
          case FLOAT  : fprintf(fp,"       real*8        ");
                        break;
          case STRING : fprintf(fp,"       character*%d  ",STR_P_SIZE);
                        break;
        }
        fprintf(fp,"%s",params[i].name->n);
        if(vsize(params[i].size)>1)
          if(params[i].type==IVEC)
            fprintf(fp,"(%d)\n",IVEC_SIZE);
          else{
            fprintf(fp,"(");
            for(j=0;j<params[i].size->dim;j++){
              fprintf(fp,"%d",params[i].size->size[j]);
              if(j<params[i].size->dim-1)
                fprintf(fp,",");
            }
            fprintf(fp,")\n");
          }
        else fprintf(fp,"\n");
        switch(params[i].type){
          case IVEC    :
          case INT    : fprintf(fp,"       common      / com_oglb_int / %s\n",params[i].name->n);
                        break;
          case FLOAT  : fprintf(fp,"       common      / com_oglb_float / %s\n",params[i].name->n);
                        break;
          case STRING : fprintf(fp,"       common      / com_oglb_char / %s\n",params[i].name->n);
                        break;
        }
        fprintf(fp,"       integer       set_%s\n",params[i].name->n);
        fprintf(fp,"       common      / com_oglb_int / set_%s\n",params[i].name->n);
      }
    }    
    fprintf(fp,"!----------------------------------------------------------------------\n");
    fprintf(fp,"!  Base lattice sizes\n");
    fprintf(fp,"!----------------------------------------------------------------------\n");
    for(j=0;j<ncoords;j++)
       for(i=0;i<coords[j].rank-1;i++){
          fprintf(fp,"       integer       %s\n",grid_base[j][i]->n);
          fprintf(fp,"       common      / com_oglb_int / %s\n",grid_base[j][i]->n);
       }
    fprintf(fp,"!----------------------------------------------------------------------\n");
    fprintf(fp,"!  Coordinate differentials\n");
    fprintf(fp,"!----------------------------------------------------------------------\n");
    for(j=0;j<ncoords;j++){
      for(res=0,i=0;i<j;i++)
        res+=(coord_difs[j][0] == coord_difs[i][0]);
      if(res==0){ /* only declare unique time coordinates */
        fprintf(fp,"       real*8        %s\n",coord_difs[j][0]->n);
        fprintf(fp,"       common      / com_oglb_float / %s\n",coord_difs[j][0]->n);
      }
      for(cl=coords[j].c_names->next,i=1;i<coords[j].rank;i++,cl=cl->next){
        fprintf(fp,"       real*8        %s\n",coord_difs[j][i]->n);
        fprintf(fp,"       common      / com_oglb_float / %s\n",coord_difs[j][i]->n);
        fprintf(fp,"       integer       %s\n",cl->name->n);
        fprintf(fp,"       common      / com_oglb_int / %s\n",cl->name->n);
      }
    }
    if(static_work){
      fprintf(fp,"!----------------------------------------------------------------------\n");
      fprintf(fp,"!  Static Work Arrays\n");
      fprintf(fp,"!----------------------------------------------------------------------\n");
      for(wl=static_work;wl!=NULL;wl=wl->next){
        fprintf(fp,"       integer       work%d, nwork%d\n",wl->work->num,
                wl->work->num);
        fprintf(fp,"       common      / com_oglb_int / work%d, nwork%d\n",
                wl->work->num,wl->work->num);
      }
    }
  }
}

void declare_parameters(FILE *fp)
{
  int i,j;
  FILE *fp1;

  if(language!=ALLF){
    for(i=0;i<nparams;i++){
      if(params[i].con!=2){
        switch(params[i].type){
          case IVEC    :
          case INT    : fprintf(fp,"  int ");
                        break;
          case FLOAT  : fprintf(fp,"  double ");
                        break;
          case STRING : fprintf(fp,"  char *");
                        break;
        }
        if(vsize(params[i].size)>1)
          fprintf(fp,"*");
        fprintf(fp,"%s;\n",params[i].name->n);
      }
      fprintf(fp,"  int set_%s;\n",params[i].name->n);
    }
    for(j=0;j<ncoords;j++){
      for(i=0;i<coords[j].rank-1;i++){
        fprintf(fp,"  int %s;\n",grid_base[j][i]->n);
      }
    }
  }else{
    fp1=fopen("sys_param.inc","w");
    if(fp1==NULL){
      fprintf(stderr,"declar_parameters: can't open sys_param.inc\n");
      fatal_error("File error");
    }
    fprintf(fp,"       integer       rmod\n");
    fprintf(fp,"       common      / com_glob_int / rmod\n");
    fprintf(fp,"!----------------------------------------------------------------------\n");
    fprintf(fp,"!  Parameters\n");
    fprintf(fp,"!----------------------------------------------------------------------\n");
    for(i=0;i<nparams;i++){
      if(params[i].con==1){
        switch(params[i].type){
          case IVEC    :  if(params[i].def && (IVEL*params[i].def_val.i_ar[0] +1 > IVEC_SIZE)){
                             fprintf(stderr,"declare_parameters: WARNING: default IVEC size must be increased.\n");
                             fprintf(stderr,"  IVEC %s needs size of %d\n",params[i].name->n,
                                  IVEL*params[i].def_val.i_ar[0] +1);
                        }
          case INT    : fprintf(fp,"       integer       ");
                        break;
          case FLOAT  : fprintf(fp,"       real*8        ");
                        break;
          case STRING : fprintf(fp,"       character*%d  ",STR_P_SIZE);
                        break;
        }
        fprintf(fp,"%s",params[i].name->n);
        if(vsize(params[i].size)>1)
          if(params[i].type==IVEC)
            fprintf(fp,"(%d)\n",IVEC_SIZE);
          else{
            fprintf(fp,"(");
            for(j=0;j<params[i].size->dim;j++){
              fprintf(fp,"%d",params[i].size->size[j]);
              if(j<params[i].size->dim-1)
                fprintf(fp,",");
            }
            fprintf(fp,")\n");
          }
        else fprintf(fp,"\n");
        switch(params[i].type){
          case IVEC    :
          case INT    : fprintf(fp,"       common      / com_glob_int / %s\n",params[i].name->n);
                        break;
          case FLOAT  : fprintf(fp,"       common      / com_glob_float / %s\n",params[i].name->n);
                        break;
          case STRING : fprintf(fp,"       common      / com_glob_char / %s\n",params[i].name->n);
                        break;
        }
        fprintf(fp,"       integer       set_%s\n",params[i].name->n);
        fprintf(fp,"       common      / com_glob_int / set_%s\n",params[i].name->n);
      }else if(params[i].con==2){
        switch(params[i].type){
          case INT    : fprintf(fp1,"       integer       ");
                        break;
          case FLOAT  : fprintf(fp1,"       real*8        ");
                        break;
          case STRING : fprintf(fp1,"       character*%d  ",STR_P_SIZE);
                        break;
        }
        fprintf(fp1,"%s\n",params[i].name->n);
        switch(params[i].type){
          case INT    : fprintf(fp1,"       parameter ( %s = %d )\n",params[i].name->n,
                        params[i].def_val.i_ar[0]);
                        break;
          case FLOAT  : fprintf(fp1,"       parameter ( %s = %g )\n",params[i].name->n,
                        params[i].def_val.f_ar[0]);
                        break;
          case STRING : fprintf(fp1,"       parameter ( %s = '%s' )\n",params[i].name->n,
                        params[i].def_val.s_ar[0]);
                        break;
        }
      }
    }
    fclose(fp1);    
  }
}

void declare_grids(FILE *fp)
{
  int r,gn;

  if(language!=ALLF){
    for(gn=0;gn<ngrids;gn++){
      for(r=0;r<grids[gn].reg.rank;r++)
        fprintf(fp,"  coords %s_%d;\n",grids[gn].name->n,r);
    }
    fprintf(fp,"  lattice_type *lats;\n");
    fprintf(fp,"  char **cname;\n\n");
  }else{
    fprintf(fp,"!----------------------------------------------------------------------\n");
    fprintf(fp,"! coordinate names\n");
    fprintf(fp,"!----------------------------------------------------------------------\n");
    fprintf(fp,"       character*%d  cname(%d)\n",STR_P_SIZE,ncoords);
    fprintf(fp,"       common      / com_glob_char / cname\n");
    fprintf(fp,"       integer       ncnames\n");
    fprintf(fp,"       common      / com_glob_int  / ncnames\n");
  }
}

void declare_gfuncs(FILE *fp)
{
  int fn,i;
  offset_type *o;
  work_list *wl;

  if(language!=ALLF){
    for(fn=0;fn<ngfuncs;fn++){
      switch(gfuncs[fn].type){
        case INT   :  if((i=resid_exists(gfuncs[fn].fname))!=-1 && resids[i].eval){
                         fprintf(fp,"  int *%s_res;\n",gfuncs[fn].fname->n);
                       }
                       for(o=gfuncs[fn].tlev;o!=NULL;o=o->next){
                         fprintf(fp,"  int *%s",gfuncs[fn].fname->n);
                         gfunc_suffix(fp,o->offset);
                         fprintf(fp,";\n");
                       }
                       break;
        case FLOAT :   if((i=resid_exists(gfuncs[fn].fname))!=-1 && resids[i].eval){
                         fprintf(fp,"  double *%s_res;\n",gfuncs[fn].fname->n);
                       }
                       for(o=gfuncs[fn].tlev;o!=NULL;o=o->next){
                         fprintf(fp,"  double *%s",gfuncs[fn].fname->n);
                         gfunc_suffix(fp,o->offset);
                         fprintf(fp,";\n");
                       }
                       break;
      }
    }
    fprintf(fp,"  char **fname;\n");       
    for(wl=static_work;wl!=NULL;wl=wl->next){
      fprintf(fp,"  double *work%d;\n",wl->work->num);
      fprintf(fp,"  int   nwork%d;\n",wl->work->num);
    }      
  }else{
    fprintf(fp,"!----------------------------------------------------------------------\n");
    fprintf(fp,"!  Grid Function Helpers\n");
    fprintf(fp,"!----------------------------------------------------------------------\n");
    fprintf(fp,"       integer       gf_ln(%d), gf_st(%d), rnpldone\n",
            get_size_one(),get_size_one());
    fprintf(fp,"       common      / com_glob_int / gf_ln, gf_st, rnpldone\n");
    fprintf(fp,"       character*%d  attrfname(%d)\n",STR_P_SIZE,get_size_one());
    fprintf(fp,"       common      / com_glob_char / attrfname\n");
  }
}

void declare_attributes(FILE *fp)
{
  int i;

  if(language!=ALLF){
    for(i=0;i<nattribs;i++){
      switch(attribs[i].type){
        case INT    : fprintf(fp,"  int *");
                      break;
        case FLOAT  : fprintf(fp,"  double *");
                      break;
        case STRING : fprintf(fp,"  char **");
                      break;
      }
      fprintf(fp,"%s;\n",attribs[i].name->n);
      fprintf(fp,"  int set_%s;\n",attribs[i].name->n);
    }
    fprintf(fp,"\n");
  }else{
    fprintf(fp,"!----------------------------------------------------------------------\n");
    fprintf(fp,"!  Attributes\n");
    fprintf(fp,"!----------------------------------------------------------------------\n");
    for(i=0;i<nattribs;i++){
      switch(attribs[i].type){
        case INT    : fprintf(fp,"       integer       %s(%d), l%s(%d)\n",attribs[i].name->n,
                              attribs[i].size,attribs[i].name->n,attribs[i].size);
                      fprintf(fp,"       common      / com_glob_int / %s, l%s\n",attribs[i].name->n,attribs[i].name->n);
                      break;
        case FLOAT  : fprintf(fp,"       real*8        %s(%d), l%s(%d)\n",attribs[i].name->n,
                              attribs[i].size,attribs[i].name->n,attribs[i].size);
                      fprintf(fp,"       common      / com_glob_float / %s, l%s\n",attribs[i].name->n,attribs[i].name->n);
                      break;
        case STRING : fprintf(fp,"       character*%d  %s(%d), l%s(%d)\n",STR_P_SIZE,attribs[i].name->n,
                              attribs[i].size,attribs[i].name->n,attribs[i].size);
                      fprintf(fp,"       common      / com_glob_char / %s, l%s\n",attribs[i].name->n,attribs[i].name->n);
                      break;
      }
      fprintf(fp,"       integer       N%s\n",attribs[i].name->n);
      fprintf(fp,"       common      / com_glob_int / N%s\n",attribs[i].name->n);
         fprintf(fp,"       integer       set_%s\n",attribs[i].name->n);
      fprintf(fp,"       common      / com_glob_int / set_%s\n",attribs[i].name->n);
    }
    fprintf(fp,"\n");
  }
}

void code_residuals(FILE *fp)
{
  switch(language){
      case IDF :
    case C   : code_residuals_c(fp);
               break;
    case UPF :
     case F77 :
     case F90 :
    case ALLF: code_residuals_f77(fp);
               break;
  }
}

void code_residuals_f77(FILE *fp)
{
  name_list *cn;
  int i,j,rn,ndo;
  char c;
  i_reg *ir;
  
  for(rn=0;rn<nresids;resids[rn++].output=0);
  for(rn=0;rn<nresids;rn++){
    if(!resids[rn].output && resids[rn].eval){
      resid_header(fp,&resids[rn]);
      cn=resids[rn].gfunc->fname;
      for(i=rn;i<nresids;i++){
        if(resids[i].gfunc->fname==cn){
          resids[i].output=1;
          for(ndo=0,c='i',ir=resids[i].reg;ir!=NULL;ir=ir->next,c++){
            if(compare_expr(ir->lower,ir->upper)){
              sprintf(forbuf,"        %c=",c);
              fort_out(fp,forbuf);
              ireg_to_string_f(fp,ir->lower,resids[i].gfunc->grd->name);
              fort_out(fp,"\n");
            }else{
              ndo++;
              sprintf(forbuf,"        do %c=",c);
              fort_out(fp,forbuf);
              ireg_to_string_f(fp,ir->lower,resids[i].gfunc->grd->name);
              fort_out(fp,", ");
              ireg_to_string_f(fp,ir->upper,resids[i].gfunc->grd->name);
              sprintf(forbuf,", %d\n",ir->inc);
              fort_out(fp,forbuf);
            }
          }
          output_resid_stat(fp,&resids[i]);
          for(j=0;j<ndo;j++)
            fort_out(fp,"        end do\n");
        }
      }
      fprintf(fp,"        return\n");
      fprintf(fp,"      end\n\n");
    }
  }
}

void code_residuals_c(FILE *fp)
{
  name_list *cn;
  int i,j,rn;
  char c;
  i_reg *ir;
  
  for(rn=0;rn<nresids;resids[rn++].output=0);
  for(rn=0;rn<nresids;rn++){
    if(!resids[rn].output && resids[rn].eval){
      resid_header(fp,&resids[rn]);
      cn=resids[rn].gfunc->fname;
      for(i=rn;i<nresids;i++){
        if(resids[i].gfunc->fname==cn){
          resids[i].output=1;
          for(c='i',ir=resids[i].reg;ir!=NULL;ir=ir->next,c++){
            if(compare_expr(ir->lower,ir->upper)){
              fprintf(fp,"  %c=",c);
              ireg_to_string(fp,ir->lower,resids[i].gfunc->grd->name);
              fprintf(fp,";{\n");
            }else{
              fprintf(fp,"  for(%c=",c);
              ireg_to_string(fp,ir->lower,resids[i].gfunc->grd->name);
              if(ir->inc>0)
                fprintf(fp,";%c<=",c);
              else fprintf(fp,";%c>=",c);
              ireg_to_string(fp,ir->upper,resids[i].gfunc->grd->name);
              fprintf(fp,";%c+=%d){\n",c,ir->inc);
            }
          }
          output_resid_stat(fp,&resids[i]);
          for(ir=resids[i].reg;ir!=NULL;ir=ir->next)
            fprintf(fp,"  }\n");
        }
      }
      fprintf(fp,"}\n\n");
    }
  }
}

void swap_top(FILE *fp)
{
  switch(language){
     case IDF :
     case UPF :
     case F77 :
     case F90 :
    case C   : swap_top_c(fp);
               break;
    case ALLF: swap_top_f77(fp);
               break;
  }
}

void swap_top_f77(FILE *fp)
{
  int rn,fn,i,cs,gf;
  char ch;
  i_reg *ir;
  gfunc_tab_list *gtl;
  
  cs=0;
  for(i=0;i<nupdates;i++)
    for(gtl=updates[i].gfs;gtl!=NULL;gtl=gtl->next)
      if(gtl->gfunc->ntlevs==3 && gtl->gfunc->tlev->next->offset==0)
        cs=1;
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"!  swap_top\n");
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"      subroutine swap_top()\n");
  fort_out(fp,"        implicit none\n\n");
  fort_out(fp,"        include 'sys_param.inc'\n");
  fort_out(fp,"        include 'gfuni0.inc'\n");
  fort_out(fp,"        include 'globals.inc'\n\n");
  fort_out(fp,"        integer tmp\n\n");
  for(rn=0;rn<nupdates;rn++)
    for(gtl=updates[rn].gfs;gtl!=NULL;gtl=gtl->next){
      if(gtl->gfunc->ntlevs==3){
        if(cs){
          gf=gfunc_to_index(gtl->gfunc);
          if(!gtl->gfunc->alias){
            sprintf(forbuf,"        tmp=ptrs(gf_st(%d)+1)\n",gf+1);
            fort_out(fp,forbuf);
            for(i=1;i<gtl->gfunc->ntlevs;i++){
              sprintf(forbuf,"        ptrs(gf_st(%d)+%d)=ptrs(gf_st(%d)+%d)\n",gf+1,i,gf+1,i+1);
              fort_out(fp,forbuf);
            }
            sprintf(forbuf,"        ptrs(gf_st(%d)+%d)=tmp\n",gf+1,i);
            fort_out(fp,forbuf);
          }else{
            for(i=1;i<gtl->gfunc->ntlevs;i++){
              sprintf(forbuf,"        ptrs(gf_st(%d)+%d)=ptrs(gf_st(%d)+%d)\n",gf+1,i,gf+1,i+1);
              fort_out(fp,forbuf);
            }
            sprintf(forbuf,"        ptrs(gf_st(%d)+%d)=ptrs(gf_st(%d)+1)\n",gf+1,i,gf+1);
            fort_out(fp,forbuf);
          }
        }else{
          gf=gfunc_to_index(gtl->gfunc);
          if(!gtl->gfunc->alias){
            sprintf(forbuf,"        tmp=ptrs(gf_st(%d)+1)\n",gf+1);
            fort_out(fp,forbuf);
            for(i=1;i<gtl->gfunc->ntlevs-1;i++){
              sprintf(forbuf,"        ptrs(gf_st(%d)+%d)=ptrs(gf_st(%d)+%d)\n",gf+1,i,gf+1,i+1);
              fort_out(fp,forbuf);
            }
            sprintf(forbuf,"        ptrs(gf_st(%d)+%d)=tmp\n",gf+1,i);
            fort_out(fp,forbuf);
          }else{
            for(i=1;i<gtl->gfunc->ntlevs-1;i++){
              sprintf(forbuf,"        ptrs(gf_st(%d)+%d)=ptrs(gf_st(%d)+%d)\n",gf+1,i,gf+1,i+1);
              fort_out(fp,forbuf);
            }
            sprintf(forbuf,"        ptrs(gf_st(%d)+%d)=ptrs(gf_st(%d)+1)\n",gf+1,i,gf+1);
            fort_out(fp,forbuf);
          }
        }
      }
    }
  fort_out(fp,"        return\n");
  fort_out(fp,"      end\n\n");
}

void swap_top_header(FILE *fp)
{
  int i,comma;
  offset_type *o;
  
  fprintf(fp,"void swap_top(");
  comma=0;
  for(i=0;i<ngfuncs;i++){
    if(gfuncs[i].ntlevs==3){
      for(o=gfuncs[i].tlev;o!=NULL;o=o->next){
         if(comma)
            fprintf(fp,",");
        switch(gfuncs[i].type){
          case INT   : fprintf(fp,"int **%s",gfuncs[i].fname->n);
                       break;
          case FLOAT : fprintf(fp,"double **%s",gfuncs[i].fname->n);
                       break;
        }
        gfunc_suffix(fp,o->offset);
        comma=1;
       }
    }
  }
  fprintf(fp,")\n{\n");
  fprintf(fp,"  int *tmpi;\n");
  fprintf(fp,"  double *tmpr;\n\n");
}

void swap_top_call(FILE *fp)
{
  int i,comma;
  offset_type *o;
  
  fprintf(fp,"swap_top(");
  comma=0;
  for(i=0;i<ngfuncs;i++){
    if(gfuncs[i].ntlevs==3){
      for(o=gfuncs[i].tlev;o!=NULL;o=o->next){
        if(comma)
           fprintf(fp,",");
        fprintf(fp,"&%s",gfuncs[i].fname->n);
        gfunc_suffix(fp,o->offset);
        comma=1;
       }
    }
  }
  fprintf(fp,");\n");
}

void swap_top_c(FILE *fp)
{
  int toffs[10],rn,fn,n,i,cs;
  char ch;
  i_reg *ir;
  offset_type *o;
  gfunc_tab_list *gtl;
  
  cs=0;
  for(i=0;i<ngfuncs;i++)
    if(gfuncs[i].ntlevs==3 && gfuncs[i].tlev->next->offset==0)
      cs=1;
  swap_top_header(fp);
  for(rn=0;rn<ngfuncs;rn++){
    if(gfuncs[rn].ntlevs==3){
      for(o=gfuncs[rn].tlev,i=0;i<gfuncs[rn].ntlevs;i++,o=o->next)
        toffs[i]=o->offset;
      if(cs){
        if(!gfuncs[rn].alias){
          switch(gfuncs[rn].type){
            case INT   : fprintf(fp,"  tmpi=*%s",gfuncs[rn].fname->n);
                         break;
            case FLOAT : fprintf(fp,"  tmpr=*%s",gfuncs[rn].fname->n);
                         break;
          }
          gfunc_suffix(fp,toffs[0]);
          fprintf(fp,";\n");
          for(i=1;i<gfuncs[rn].ntlevs;i++){
            fprintf(fp,"  *%s",gfuncs[rn].fname->n);
            gfunc_suffix(fp,toffs[i-1]);
            fprintf(fp,"=*%s",gfuncs[rn].fname->n);
            gfunc_suffix(fp,toffs[i]);
            fprintf(fp,";\n");
          }
          fprintf(fp,"  *%s",gfuncs[rn].fname->n);
          gfunc_suffix(fp,toffs[i-1]);
          switch(gfuncs[rn].type){
            case INT   : fprintf(fp,"=tmpi;\n");
                         break;
            case FLOAT : fprintf(fp,"=tmpr;\n");
                         break;
          }
        }else{
          for(i=1;i<gfuncs[rn].ntlevs;i++){
            fprintf(fp,"  *%s",gfuncs[rn].fname->n);
            gfunc_suffix(fp,toffs[i-1]);
            fprintf(fp,"=*%s",gfuncs[rn].fname->n);
            gfunc_suffix(fp,toffs[i]);
            fprintf(fp,";\n");
          }
          fprintf(fp,"  *%s",gfuncs[rn].fname->n);
          gfunc_suffix(fp,toffs[i-1]);
          fprintf(fp,"=*%s",gfuncs[rn].fname->n);
          gfunc_suffix(fp,toffs[0]);
          fprintf(fp,";\n");
        }
      }else{
        if(!gfuncs[rn].alias){
          switch(gfuncs[rn].type){
            case INT   : fprintf(fp,"  tmpi=*%s",gfuncs[rn].fname->n);
                         break;
            case FLOAT : fprintf(fp,"  tmpr=*%s",gfuncs[rn].fname->n);
                         break;
          }
          gfunc_suffix(fp,toffs[0]);
          fprintf(fp,";\n");
          for(i=1;i<gfuncs[rn].ntlevs-1;i++){
            fprintf(fp,"  *%s",gfuncs[rn].fname->n);
            gfunc_suffix(fp,toffs[i-1]);
            fprintf(fp,"=*%s",gfuncs[rn].fname->n);
            gfunc_suffix(fp,toffs[i]);
            fprintf(fp,";\n");
          }
          fprintf(fp,"  *%s",gfuncs[rn].fname->n);
          gfunc_suffix(fp,toffs[i-1]);
          switch(gfuncs[rn].type){
            case INT   : fprintf(fp,"=tmpi;\n");
                         break;
            case FLOAT : fprintf(fp,"=tmpr;\n");
                         break;
          }
        }else{
          for(i=1;i<gfuncs[rn].ntlevs-1;i++){
            fprintf(fp,"  *%s",gfuncs[rn].fname->n);
            gfunc_suffix(fp,toffs[i-1]);
            fprintf(fp,"=*%s",gfuncs[rn].fname->n);
            gfunc_suffix(fp,toffs[i]);
            fprintf(fp,";\n");
          }
          fprintf(fp,"  *%s",gfuncs[rn].fname->n);
          gfunc_suffix(fp,toffs[i-1]);
          fprintf(fp,"=*%s",gfuncs[rn].fname->n);
          gfunc_suffix(fp,toffs[0]);
          fprintf(fp,";\n");
        }
      }
    }
  }
  fprintf(fp,"}\n\n");
}

void swap_levels(FILE *fp)
{
  switch(language){
     case UPF :
     case IDF :
     case F77 :
     case F90 :
    case C   : swap_levels_c(fp);
               break;
    case ALLF: swap_levels_f77(fp);
               break;
  }
}

void swap_levels_f77(FILE *fp)
{
  int rn,fn,i;
  char ch;
  i_reg *ir;
  gfunc_tab_list *gtl;

  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"!  swap_levels\n");
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"      subroutine swap_levels()\n");
  fort_out(fp,"        implicit none\n\n");
  fort_out(fp,"        include 'sys_param.inc'\n");
  fort_out(fp,"        include 'gfuni0.inc'\n");
  fort_out(fp,"        include 'globals.inc'\n\n");
  fort_out(fp,"        integer tmp\n\n");
  for(rn=0;rn<nupdates;rn++)
    for(gtl=updates[rn].gfs;gtl!=NULL;gtl=gtl->next){
      int gf,k,i;
      if(gtl->gfunc->ntlevs>1){
        k=gtl->gfunc->ntlevs-1;
        gf=gfunc_to_index(gtl->gfunc);
        if(!gtl->gfunc->alias){
          sprintf(forbuf,"        tmp=ptrs(gf_st(%d)+%d)\n",gf+1,k+1);
          fort_out(fp,forbuf);
          for(i=1;i<gtl->gfunc->ntlevs;i++){
            sprintf(forbuf,"        ptrs(gf_st(%d)+%d)=ptrs(gf_st(%d)+%d)\n",
                    gf+1,k-i+2,gf+1,k-i+1);
            fort_out(fp,forbuf);
          }
          sprintf(forbuf,"        ptrs(gf_st(%d)+1)=tmp\n",gf+1);
          fort_out(fp,forbuf);
        }else{
          for(i=1;i<gtl->gfunc->ntlevs;i++){
            sprintf(forbuf,"        ptrs(gf_st(%d)+%d)=ptrs(gf_st(%d)+%d)\n",
                    gf+1,k-i+2,gf+1,k-i+1);
            fort_out(fp,forbuf);
          }
          sprintf(forbuf,"        ptrs(gf_st(%d)+1)=ptrs(gf_st(%d)+%d)\n",gf+1,gf+1,k+1);
          fort_out(fp,forbuf);
        }
      }
    }
  fort_out(fp,"        return\n");
  fort_out(fp,"      end\n\n");
}

void swap_header(FILE *fp)
{
  int i,comma;
  offset_type *o;
  
  fprintf(fp,"void swap_levels(");
  comma=0;
  for(i=0;i<ngfuncs;i++){
    if(gfuncs[i].ntlevs>1){
      for(o=gfuncs[i].tlev;o!=NULL;o=o->next){
         if(comma) fprintf(fp,",");
        switch(gfuncs[i].type){
          case INT   : fprintf(fp,"int **%s",gfuncs[i].fname->n);
                       break;
          case FLOAT : fprintf(fp,"double **%s",gfuncs[i].fname->n);
                       break;
        }
        gfunc_suffix(fp,o->offset);
        comma=1;
       }
    }
  }
  fprintf(fp,")\n{\n");
  fprintf(fp,"  int *tmpi;\n");
  fprintf(fp,"  double *tmpr;\n\n");
}

void swap_call(FILE *fp)
{
  int i,comma;
  offset_type *o;
  
  fprintf(fp,"swap_levels(");
  comma=0;
  for(i=0;i<ngfuncs;i++){
    if(gfuncs[i].ntlevs>1){
      for(o=gfuncs[i].tlev;o!=NULL;o=o->next){
         if(comma) fprintf(fp,",");
        fprintf(fp,"&%s",gfuncs[i].fname->n);
        gfunc_suffix(fp,o->offset);
        comma=1;
       }
    }
  }
  fprintf(fp,");\n");
}

void swap_levels_c(FILE *fp)
{
  int rn,fn,n,i;
  char ch;
  i_reg *ir;
  offset_type *o;
  int toffs[10];

   swap_header(fp);
  for(rn=0;rn<ngfuncs;rn++){
    if(gfuncs[rn].ntlevs>1){
      for(o=gfuncs[rn].tlev,i=gfuncs[rn].ntlevs-1;i>=0;i--,o=o->next)
        toffs[i]=o->offset;
      if(!gfuncs[rn].alias){
        switch(gfuncs[rn].type){
          case INT   : fprintf(fp,"  tmpi=*%s",gfuncs[rn].fname->n);
                       break;
          case FLOAT : fprintf(fp,"  tmpr=*%s",gfuncs[rn].fname->n);
                       break;
        }
        gfunc_suffix(fp,toffs[0]);
        fprintf(fp,";\n");
        for(i=1;i<gfuncs[rn].ntlevs;i++){
          fprintf(fp,"  *%s",gfuncs[rn].fname->n);
          gfunc_suffix(fp,toffs[i-1]);
          fprintf(fp,"=*%s",gfuncs[rn].fname->n);
          gfunc_suffix(fp,toffs[i]);
          fprintf(fp,";\n");
        }
        fprintf(fp,"  *%s",gfuncs[rn].fname->n);
        gfunc_suffix(fp,toffs[i-1]);
        switch(gfuncs[rn].type){
          case INT   : fprintf(fp,"=tmpi;\n");
                       break;
          case FLOAT : fprintf(fp,"=tmpr;\n");
                       break;
        }
      }else{
        for(i=1;i<gfuncs[rn].ntlevs;i++){
          fprintf(fp,"  *%s",gfuncs[rn].fname->n);
          gfunc_suffix(fp,toffs[i-1]);
          fprintf(fp,"=*%s",gfuncs[rn].fname->n);
          gfunc_suffix(fp,toffs[i]);
          fprintf(fp,";\n");
        }
        fprintf(fp,"  *%s",gfuncs[rn].fname->n);
        gfunc_suffix(fp,toffs[i-1]);
        fprintf(fp,"=*%s",gfuncs[rn].fname->n);
        gfunc_suffix(fp,toffs[0]);
        fprintf(fp,";\n");
      }
    }
  }
  fprintf(fp,"}\n\n");
}

void one_step(FILE *fp)
{
  switch(language){
     case IDF :
    case C   : one_step_c(fp);
               break;
    case UPF :
    case ALLF:
    case F90 :
    case F77 : one_step_f77(fp);
               break;
  }
}

void one_step_f77(FILE *fp)
{
  int rn,fn,i,j,upn,ndo,ndim;
  char ch,k;
  i_reg *ir,*irl[MAXRANK];
  gfunc_ref_list *grl;
  gfunc_tab_list *gtl,*gl;
  node *m,*d,*gf;
  index_list *o;
  grid_table *oldgrd;

  for(rn=0;rn<nresids;resids[rn++].output=0);
  for(upn=0;upn<nupdates;upn++){ /* loop over updates */
    update_header(fp,&updates[upn]);
    if(!mystrcmp(updates[upn].type->n,"auto")){ /* auto (point-wise Newton's iteration) */
      for(gtl=updates[upn].gfs;gtl!=NULL;gtl=gtl->next){ /* loop over grid functions to be updated */
        rn=resid_exists(gtl->gfunc->fname);
        if(rn==-1){
          fprintf(stderr,"Auto update exists for %s but no residual.\n",gtl->gfunc->fname->n);
          fatal_error("Bad update declaration");
        }
        for(;rn<nresids;rn++){ /* loop over regions */
          if(!resids[rn].output && resids[rn].gfunc == gtl->gfunc){ /* this region has not been output */
            resids[rn].output=1; /* now it has */
            ndo=0;
            for(ndim=0,ir=resids[rn].reg,i=0;ir!=NULL;ndim++,ir=ir->next,i++)
              irl[i]=ir;
            for(k='i'+ndim-1,i=ndim-1;i>=0;k--,i--){
              ir=irl[i];
              if(compare_expr(ir->lower,ir->upper)){
                sprintf(forbuf,"          %c=",k);
                fort_out(fp,forbuf);
                ireg_to_string_f(fp,ir->lower,resids[rn].gfunc->grd->name);
              }else{
                ndo++;
                sprintf(forbuf,"          do %c=",k);
                fort_out(fp,forbuf);
                ireg_to_string_f(fp,ir->lower,resids[rn].gfunc->grd->name);
                fort_out(fp,", ");
                ireg_to_string_f(fp,ir->upper,resids[rn].gfunc->grd->name);
                sprintf(forbuf,", %d",ir->inc);
                fort_out(fp,forbuf);
              }
              fort_out(fp,"\n");
            }
            output_update_stat(fp,&resids[rn]);
            for(i=0;i<ndo;i++)
              fort_out(fp,"          end do\n");
          }
        }
      }
    }else if(mystrcmp(updates[upn].type->n,"stub")){
      FILE *ne;
      char nebuf[1025];
      
      ne=fopen(updates[upn].type->n,"r");
      if(ne==NULL)
        fprintf(stderr,"Unable to open file %s.\n",updates[upn].type->n);
      else{
        while(fgets(nebuf,1024,ne))
          fprintf(fp,"%s",nebuf);
        fclose(ne);
      }
    }  
    fprintf(fp,"        return\n");
    fprintf(fp,"      end\n\n");
  }
}

void one_step_c(FILE *fp)
{
  int rn,fn,i,j,upn;
  char ch,k;
  i_reg *ir;
  gfunc_ref_list *grl;
  gfunc_tab_list *gtl,*gl;
  node *m,*d,*gf;
  index_list *o;
  
  for(rn=0;rn<nresids;resids[rn++].output=0);
  for(upn=0;upn<nupdates;upn++){
    update_header(fp,&updates[upn]);
    if(!mystrcmp(updates[upn].type->n,"auto")){
      for(gtl=updates[upn].gfs;gtl!=NULL;gtl=gtl->next){
        rn=resid_exists(gtl->gfunc->fname);
#ifdef RNPLDEBUG
fprintf(stderr,"one_step_c: rn=%d\n",rn);
#endif
       if(rn==-1){
          fprintf(stderr,"Auto update exists for %s but no residual.\n",gtl->gfunc->fname->n);
          fatal_error("Bad update declaration");
        }
        for(;rn<nresids;rn++){
          if(!resids[rn].output && resids[rn].gfunc == gtl->gfunc){
            resids[rn].output=1;
            for(k='i',ir=resids[rn].reg;ir!=NULL;k++,ir=ir->next){
              if(compare_expr(ir->lower,ir->upper)){
                fprintf(fp,"  %c=",k);
                ireg_to_string(fp,ir->lower,resids[rn].gfunc->grd->name);
                fprintf(fp,";{\n");
              }else{
                fprintf(fp,"  for(%c=",k);
                ireg_to_string(fp,ir->lower,resids[rn].gfunc->grd->name);
                if(ir->inc>0)
                  fprintf(fp,";%c<=",k);
                else fprintf(fp,";%c>=",k);
                ireg_to_string(fp,ir->upper,resids[rn].gfunc->grd->name);
                fprintf(fp,";%c+=%d){\n",k,ir->inc);
              }
            }
            output_update_stat(fp,&resids[rn]);
            for(ir=resids[rn].reg;ir!=NULL;ir=ir->next)
              fprintf(fp,"  }\n");
          }
        }
      }
    }else if(mystrcmp(updates[upn].type->n,"stub")){
      FILE *ne;
      char nebuf[1025];
      
      ne=fopen(updates[upn].type->n,"r");
      if(ne==NULL)
        fprintf(stderr,"Unable to open file %s.\n",updates[upn].type->n);
      else{
        while(fgets(nebuf,1024,ne))
          fprintf(fp,"%s",nebuf);
        fclose(ne);
      }
    }  
    fprintf(fp,"}\n\n");
  }
}

void update_helpers(FILE *fp)
{
  switch(language){
     case IDF :
     case UPF :
     case F90 :
     case F77 :
    case C   : update_helpers_c(fp);
               break;
    case ALLF: update_helpers_f77(fp);
               break;
  }
}

void update_helpers_f77(FILE *fp)
{
  int rn,fn,i,j,it,ngftlvs;
  char k;
  i_reg *ir;
  int rev;
  gfunc_table *gf;
  gfunc_tab_list *gtl,*fcn;
  offset_type *o;
  work_list *wk;
  
  /* sets most advanced time level to most advanced - 1 */
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"!  initial_guess\n");
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"      subroutine initial_guess()\n");
  fort_out(fp,"        implicit none\n\n");
  fort_out(fp,"        include 'sys_param.inc'\n");
  fort_out(fp,"        include 'gfuni0.inc'\n");
  fort_out(fp,"        include 'other_glbs.inc'\n");
  fort_out(fp,"        include 'globals.inc'\n\n");
  fort_out(fp,"        integer i,j,k\n");
  for(rn=0;rn<nupdates;rn++)
    for(gtl=updates[rn].gfs;gtl!=NULL;gtl=gtl->next){
      if(gtl->gfunc->ntlevs>1){
        fort_out(fp,"        call rdvcpy(");
        sprintf(forbuf,"q(ptrs(gf_st(%d)+1)), ",gfunc_to_index(gtl->gfunc)+1);
        fort_out(fp,forbuf);
        sprintf(forbuf,"q(ptrs(gf_st(%d)+2)),",gfunc_to_index(gtl->gfunc)+1);
        fort_out(fp,forbuf);
        grid_size(fp,gtl->gfunc->grd);
        fort_out(fp,")\n");
      }
    }
  fort_out(fp,"        return\n");
  fort_out(fp,"      end\n\n");
  
  for(ngftlvs=0,i=0;i<ngfuncs;i++)
    ngftlvs+=gfuncs[i].ntlevs;
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"!  calc_resid\n");
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"      real*8 function calc_resid()\n");
  fort_out(fp,"        implicit none\n\n");
  fort_out(fp,"        include 'sys_param.inc'\n");
  fort_out(fp,"        include 'gfuni0.inc'\n");
  fort_out(fp,"        include 'globals.inc'\n\n");
  fort_out(fp,"        real*8   u,l2norm\n");
  for(rev=0,i=0;i<nresids;){
    gf=resids[i].gfunc;
    if(resids[i].eval)
      rev++;
    while(i<nresids && gf==resids[i].gfunc)
      i++;
  }
  if(rev==0)
    fprintf(stderr,"warning: calc_resid: No residual is being evaluated.\n");
  else{
    fort_out(fp,"        real*8   ");
    for(i=0;i<rev;i++){
      sprintf(forbuf,"u%d,v%d",i,i);
      fort_out(fp,forbuf);
      if(i<rev-1)
        fort_out(fp,",");
      else fort_out(fp,"\n");
    }
    for(j=0,i=0;i<nresids;){
      gf=resids[i].gfunc;
      if(resids[i].eval){
        sprintf(forbuf,"        u%d=l2norm(",j);
        fort_out(fp,forbuf);
        grid_size(fp,resids[i].gfunc->grd);
        sprintf(forbuf,",q(ptrs(gf_st(%d)+1)))\n",ngfuncs+j+1);
        fort_out(fp,forbuf);
        sprintf(forbuf,"        v%d=l2norm(",j);
                                fort_out(fp,forbuf);
        grid_size(fp,resids[i].gfunc->grd);
        sprintf(forbuf,",q(ptrs(gf_st(%d)+1)))\n",gfunc_to_index(resids[i].gfunc)+1);
        fort_out(fp,forbuf);
        sprintf(forbuf,"        if (v%d .ne. 0.0) then\n",j);
        fort_out(fp,forbuf);
        sprintf(forbuf,"          u%d = u%d/v%d\n",j,j,j);
        fort_out(fp,forbuf);
        fort_out(fp,"        end if\n");
        j++;
      }
      while(i<nresids && gf==resids[i].gfunc)
        i++;
    }
    fort_out(fp,"          u=");
    for(i=0;i<rev;i++){
      sprintf(forbuf,"u%d",i);
      fort_out(fp,forbuf);
      if(i<rev-1)
        fort_out(fp,"+");
      else fort_out(fp,"\n");
    }
  }
  fort_out(fp,"        calc_resid=u\n");
  fort_out(fp,"        return\n");
  fort_out(fp,"      end\n\n");

  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"!  take_step\n");
  fort_out(fp,"!----------------------------------------------------------------------\n");
  sprintf(forbuf,"      subroutine take_step(%s)\n",coords[0].c_names->name->n);
  fort_out(fp,forbuf);
  fort_out(fp,"        implicit none\n\n");
  fort_out(fp,"        include 'sys_param.inc'\n");
  fort_out(fp,"        include 'gfuni0.inc'\n");
  fort_out(fp,"        include 'other_glbs.inc'\n");
  fort_out(fp,"        include 'globals.inc'\n\n");
  fort_out(fp,"        integer mmaloc, mmdeal\n");
  sprintf(forbuf,"        real*8  %s\n\n",coords[0].c_names->name->n);
  fort_out(fp,forbuf);
  for(rn=0;rn<nupdates;rn++){
    for(wk=updates[rn].work_refs;wk!=NULL;wk=wk->next){
      if(wk->work->ato){
        sprintf(forbuf,"        integer work%d, nwork%d\n",wk->work->num,
                wk->work->num);
        fort_out(fp,forbuf);
      }
    }
  }
  for(rn=0;rn<nupdates;rn++){
    for(wk=updates[rn].work_refs;wk!=NULL;wk=wk->next){
      if(wk->work->ato){
        sprintf(forbuf,"        nwork%d=",wk->work->num);
        fort_out(fp,forbuf);
        ex_to_string_f(fp,wk->work->expr);
        fort_out(fp,"\n");
        sprintf(forbuf,"        work%d=mmaloc(nwork%d)\n",wk->work->num,
                wk->work->num);
        fort_out(fp,forbuf);
        sprintf(forbuf,"        if(work%d .eq. -1) then\n",wk->work->num);
        fort_out(fp,forbuf);
        fort_out(fp,"          write(*,*)'ERROR: unable to allocate work array'\n");
        fort_out(fp,"          write(*,*)'increase memsiz and recompile'\n");
        fort_out(fp,"          stop\n");
        fort_out(fp,"        end if\n");
      }
    }
    fort_out(fp,"        ");
    update_call(fp,&updates[rn]);
    for(wk=updates[rn].work_refs;wk!=NULL;wk=wk->next){
      if(wk->work->ato){
        fort_out(fp,"        ");
        sprintf(forbuf,"work%d=mmdeal(work%d,nwork%d)\n",wk->work->num,
                wk->work->num,wk->work->num);
        fort_out(fp,forbuf);
      }
    }
  }
  for(rn=0;rn<nresids;){
    if(resids[rn].eval!=0){
      fort_out(fp,"        ");
      resid_call(fp,&resids[rn]);
    }
    for(rn++;rn<nresids && resids[rn].gfunc==resids[rn-1].gfunc;rn++);
  }
  fort_out(fp,"        return\n");
  fort_out(fp,"      end\n\n");
}

void initial_guess_proto(FILE *fp)
{
   int rn,i;
  gfunc_ref_list *gl;
  coord_list *cl;
  
  fprintf(fp,"void initial_guess(");
  for(gl=update_glob_gf;gl!=NULL;gl=gl->next){
    if(gl->gfunc->ntlevs>1){
       switch(gl->gfunc->type){
          case INT :
             fprintf(fp,"int *%s",gl->gfunc->fname->n);
             gfunc_suffix(fp,gl->gfunc->tlev->offset);
             fprintf(fp,",int *%s",gl->gfunc->fname->n);
             gfunc_suffix(fp,gl->gfunc->tlev->next->offset);
             fprintf(fp,",");
             break;
          case FLOAT :
             fprintf(fp,"double *%s",gl->gfunc->fname->n);
             gfunc_suffix(fp,gl->gfunc->tlev->offset);
             fprintf(fp,",double *%s",gl->gfunc->fname->n);
             gfunc_suffix(fp,gl->gfunc->tlev->next->offset);
             fprintf(fp,",");
             break;
       }
    }
  }
  for(rn=0;rn<ngrids;rn++){
    for(cl=grids[rn].clst;cl!=NULL;cl=cl->next){
      fprintf(fp,"int %s_N%s",grids[rn].name->n,cl->name->n);
      if(cl->next) fprintf(fp,",");
    }
    if(rn<ngrids-1) fprintf(fp,",");
   }
  fprintf(fp,")");
}

void initial_guess_call(FILE *fp)
{
   int rn;
  gfunc_ref_list *gl;
  coord_list *cl;
  
  fprintf(fp,"initial_guess(");
  for(gl=update_glob_gf;gl!=NULL;gl=gl->next){
    if(gl->gfunc->ntlevs>1){
         fprintf(fp,"%s",gl->gfunc->fname->n);
         gfunc_suffix(fp,gl->gfunc->tlev->offset);
         fprintf(fp,",%s",gl->gfunc->fname->n);
         gfunc_suffix(fp,gl->gfunc->tlev->next->offset);
         fprintf(fp,",");
    }
  }
  for(rn=0;rn<ngrids;rn++){
    for(cl=grids[rn].clst;cl!=NULL;cl=cl->next){
      fprintf(fp,"%s_N%s",grids[rn].name->n,cl->name->n);
      if(cl->next) fprintf(fp,",");
    }
    if(rn<ngrids-1) fprintf(fp,",");
   }
  fprintf(fp,");\n");
}

void calc_resid_proto(FILE *fp)
{
  int i;
  gfunc_table *gf;
  coord_list *cl;

  fprintf(fp,"double calc_resid(");
  for(i=0;i<nresids;){
     gf=resids[i].gfunc;
     if(resids[i].eval){
        switch(resids[i].gfunc->type){
          case INT :
             fprintf(fp,"int *%s_res,",resids[i].gfunc->fname->n);
             fprintf(fp,"int *%s",resids[i].gfunc->fname->n);
             gfunc_suffix(fp,resids[i].gfunc->tlev->offset);
             fprintf(fp,",");
             break;
          case FLOAT :
             fprintf(fp,"double *%s_res,",resids[i].gfunc->fname->n);
             fprintf(fp,"double *%s",resids[i].gfunc->fname->n);
             gfunc_suffix(fp,resids[i].gfunc->tlev->offset);
             fprintf(fp,",");
             break;
        }
     }
    while(i<nresids && gf==resids[i].gfunc)
      i++;
  }
  for(i=0;i<ngrids;i++){
    for(cl=grids[i].clst;cl!=NULL;cl=cl->next){
      fprintf(fp,"int %s_N%s",grids[i].name->n,cl->name->n);
      if(cl->next || i<ngrids-1)
         fprintf(fp,",");
    }
  }
  fprintf(fp,")");
}

void calc_resid_call(FILE *fp)
{
  int i;
  gfunc_table *gf;
  coord_list *cl;

  fprintf(fp,"calc_resid(");
  for(i=0;i<nresids;){
     gf=resids[i].gfunc;
     if(resids[i].eval){
         fprintf(fp,"%s_res,",resids[i].gfunc->fname->n);
         fprintf(fp,"%s",resids[i].gfunc->fname->n);
         gfunc_suffix(fp,resids[i].gfunc->tlev->offset);
       fprintf(fp,",");
     }
    while(i<nresids && gf==resids[i].gfunc)
      i++;
  }
  for(i=0;i<ngrids;i++){
    for(cl=grids[i].clst;cl!=NULL;cl=cl->next){
      fprintf(fp,"%s_N%s",grids[i].name->n,cl->name->n);
      if(cl->next || i<ngrids-1)
         fprintf(fp,",");
    }
  }
  fprintf(fp,");\n");
}

void take_step_proto(FILE *fp)
{
  gfunc_ref_list *gl;
  coord_list *cl;
  i_reg *ir;
  int i,j,rn,fl,used_time=0;
  gfunc_tab_list *gt;
  param_ref_list *pr;
  gfunc_table *gf;
  work_list *wl;
  
  fprintf(fp,"void take_step(int *rnpldone,");
  for(i=0;i<nresids;){
     gf=resids[i].gfunc;
     if(resids[i].eval){
        switch(resids[i].gfunc->type){
          case INT :
             fprintf(fp,"int *%s_res,",resids[i].gfunc->fname->n);
             break;
          case FLOAT :
             fprintf(fp,"double *%s_res,",resids[i].gfunc->fname->n);
             break;
        }
     }
    while(i<nresids && gf==resids[i].gfunc)
      i++;
  }
  for(gl=update_glob_gf;gl!=NULL;gl=gl->next){
    switch(gl->gfunc->type){
      case INT   : fprintf(fp,"int *");
                   break;
      case FLOAT : fprintf(fp,"double *");
                   break;
    }
    fprintf(fp,"%s",gl->gfunc->fname->n);
    gfunc_suffix(fp,gl->toff);
    if((gl->next && gl->next->gfunc->grd != gl->gfunc->grd) || !gl->next){
      for(i=0,cl=gl->gfunc->grd->clst;i<gl->gfunc->grd->reg.rank;i++,cl=cl->next){
        fprintf(fp,",int %s_N%s",gl->gfunc->grd->name->n,cl->name->n);
      }
    }
    if(gl->next) fprintf(fp,",");
  }
  for(cl=update_glob_crds;cl!=NULL;cl=cl->next){
    if(is_space(cl->name))
      fprintf(fp,",double *%s",cl->name->n);
    else{
        fprintf(fp,",double %s",cl->name->n);
        if(cl->name==coords[0].c_names->name)
          used_time=1;
    }
  }
  for(cl=update_glob_cdifs;cl!=NULL;cl=cl->next){
    fprintf(fp,",double %s",cl->name->n);
  }
  for(pr=update_glob_par;pr!=NULL;pr=pr->next){
    switch(pr->par->type){
      case  INT    :  fprintf(fp,",int ");
                    break;
      case  FLOAT  :  fprintf(fp,",double ");
                    break;
    }
    if(vsize(pr->par->size)>1)
      fprintf(fp,"*");
    fprintf(fp,"%s",pr->par->name->n);
  }
   if(!used_time)
     fprintf(fp,",double %s",coords[0].c_names->name->n);
  for(wl=static_work;wl!=NULL;wl=wl->next){
     fprintf(fp,",double *work%d,int nwork%d",wl->work->num,wl->work->num);
  }
  fprintf(fp,")");
}

void take_step_call(FILE *fp)
{
  gfunc_ref_list *gl;
  coord_list *cl;
  i_reg *ir;
  int i,j,rn,fl,used_time=0;
  gfunc_tab_list *gt;
  param_ref_list *pr;
  gfunc_table *gf;
  work_list *wl;
  
  fprintf(fp,"take_step(rnpldone,");
  for(i=0;i<nresids;){
     gf=resids[i].gfunc;
     if(resids[i].eval){
       fprintf(fp,"%s_res,",resids[i].gfunc->fname->n);
     }
    while(i<nresids && gf==resids[i].gfunc)
      i++;
  }
  for(gl=update_glob_gf;gl!=NULL;gl=gl->next){
    fprintf(fp,"%s",gl->gfunc->fname->n);
    gfunc_suffix(fp,gl->toff);
    if((gl->next && gl->next->gfunc->grd != gl->gfunc->grd) || !gl->next){
      for(i=0,cl=gl->gfunc->grd->clst;i<gl->gfunc->grd->reg.rank;i++,cl=cl->next){
        fprintf(fp,",%s_N%s",gl->gfunc->grd->name->n,cl->name->n);
      }
    }
    if(gl->next) fprintf(fp,",");
  }
  for(cl=update_glob_crds;cl!=NULL;cl=cl->next){
    if(is_space(cl->name))
      fprintf(fp,",%s",cl->name->n);
    else{
        fprintf(fp,",%s",cl->name->n);
        if(cl->name==coords[0].c_names->name)
          used_time=1;
    }
  }
  for(cl=update_glob_cdifs;cl!=NULL;cl=cl->next){
    fprintf(fp,",%s",cl->name->n);
  }
  for(pr=update_glob_par;pr!=NULL;pr=pr->next){
    fprintf(fp,",%s",pr->par->name->n);
  }
   if(!used_time)
     fprintf(fp,",%s",coords[0].c_names->name->n);
  for(wl=static_work;wl!=NULL;wl=wl->next){
     fprintf(fp,",work%d,nwork%d",wl->work->num,wl->work->num);
  }
  fprintf(fp,");\n");
}

void update_helpers_c(FILE *fp)
{
  int rn,fn,i,j,it;
  char k;
  i_reg *ir;
  gfunc_tab_list *gtl,*fcn;
  int rev;
  gfunc_table *gf;
  offset_type *o;
  work_list *wk;
  
   /* calc_resid */
   calc_resid_proto(fp);
  fprintf(fp,"\n{\n  double rnplu;\n");
  for(rev=0,i=0;i<nresids;){
    gf=resids[i].gfunc;
    if(resids[i].eval)
      rev++;
    while(i<nresids && gf==resids[i].gfunc)
      i++;
  }
  if(rev==0){
    fprintf(stderr,"warning: calc_resid: No residual is being evaluated.\n");
     fprintf(fp,"  return 0.0;\n");
  }else{
    fprintf(fp,"  double ");
    for(i=0;i<rev;i++){
      fprintf(fp,"rnplu%d=0.0,rnplv%d",i,i);
      if(i<rev-1)
        fprintf(fp,",");
      else fprintf(fp,";\n");
    }
    for(j=0,i=0;i<nresids;){
      gf=resids[i].gfunc;
      if(resids[i].eval){
        fprintf(fp,"  rnplu%d=l2norm(",j);
        grid_size(fp,resids[i].gfunc->grd);
        fprintf(fp,",%s_res);\n",resids[i].gfunc->fname->n);
        fprintf(fp,"  rnplv%d=l2norm(",j);
        grid_size(fp,resids[i].gfunc->grd);
        fprintf(fp,",%s",resids[i].gfunc->fname->n);
        gfunc_suffix(fp,resids[i].gfunc->tlev->offset);
        fprintf(fp,");\n");
        fprintf(fp,"  if(rnplv%d!=0.0) rnplu%d/=rnplv%d;\n",j,j,j);
        j++;
      }
      while(i<nresids && gf==resids[i].gfunc)
        i++;
    }
     fprintf(fp,"  rnplu=");
     for(i=0;i<rev;i++){
       fprintf(fp,"rnplu%d",i);
       if(i<rev-1)
         fprintf(fp,"+");
       else fprintf(fp,";\n");
     }
     fprintf(fp,"  return rnplu;\n");
  }
  fprintf(fp,"}\n\n");

  /* take_step */
  take_step_proto(fp);
  fprintf(fp,"\n{\n");
  for(wk=update_auto_work;wk!=NULL;wk=wk->next){
    if(wk->work->ato){
      fprintf(fp,"  double *work%d;\n",wk->work->num);
      fprintf(fp,"  int nwork%d;\n",wk->work->num);
    }
  }
  fprintf(fp,"\n");
  for(rn=0;rn<nupdates;rn++){
     for(wk=updates[rn].work_refs;wk!=NULL;wk=wk->next){
       if(wk->work->ato){
         fprintf(fp,"  nwork%d=",wk->work->num);
         ex_to_string_wk(fp,wk->work->expr);
         fprintf(fp,";\n");
         fprintf(fp,"  work%d=vec_alloc_n(nwork%d,\"work%d\");\n",wk->work->num,
         wk->work->num,wk->work->num);
       }
    }
    fprintf(fp,"  ");
    update_call(fp,&updates[rn]);
    for(wk=updates[rn].work_refs;wk!=NULL;wk=wk->next){
      if(wk->work->ato){
        fprintf(fp,"  free(work%d);\n",wk->work->num);
      }
    }
  }
  for(rn=0;rn<nresids;){
    if(resids[rn].eval!=0){
      fprintf(fp,"  ");
      resid_call(fp,&resids[rn]);
    }
    for(rn++;rn<nresids && resids[rn].gfunc==resids[rn-1].gfunc;rn++);
  }
  fprintf(fp,"}\n\n");
}

void code_updates(FILE *fp)
{

  update_helpers(fp);
  if(!strcmp("standard",loop_driver->n)){
    switch(language){
       case IDF :
       case UPF :
       case F77 :
       case F90 :
      case C   : make_stand_loop_c(fp);
                 break;
      case ALLF: make_stand_loop_f77(fp);
                 break;
    }
  }else if(!strcmp("iterative",loop_driver->n)){
    switch(language){
       case IDF :
       case UPF :
       case F77 :
       case F90 :
      case C   : make_iter_loop_c(fp);
                 break;
      case ALLF: make_iter_loop_f77(fp);
                 break;
    }
  }else{
    fprintf(stderr,"Unknown looper type <%s>.\n",loop_driver->n);
    fatal_error("Unknown looper.");
  }
  fprintf(fp,"\n");
}

void init_params_attribs(FILE *fp)
{
  switch(language){
     case IDF :
     case UPF :
     case F77 :
     case F90 :
    case C   : init_params_attribs_c(fp);
               break;
    case ALLF: init_params_attribs_f77(fp);
               break;
  }
}

void init_params_attribs_f77(FILE *fp)
{
  int i,j,k,p,l,*ind,done;

  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"!  init_params_attribs\n");
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"      subroutine init_params_attribs()\n");
  fort_out(fp,"        implicit none\n\n");
  fort_out(fp,"        include 'sys_param.inc'\n");
  fort_out(fp,"        include 'gfuni0.inc'\n");
  fort_out(fp,"        include 'other_glbs.inc'\n");
  fort_out(fp,"        include 'globals.inc'\n\n");
  for(i=0;i<nparams;i++){
     if(params[i].con!=2){
        sprintf(forbuf,"        set_%s=0\n",params[i].name->n);
        fort_out(fp,forbuf);
       if(vsize(params[i].size)==1){
         if(params[i].def){
           p=sprintf(forbuf,"        %s=",params[i].name->n);
           switch(params[i].type){
             case INT   : p+=sprintf(forbuf+p,"%d",params[i].def_val.i_ar[0]);
                          break;
             case FLOAT : p+=fort_out_const(forbuf+p,params[i].def_val.f_ar[0]);
                          break;
             case STRING: if(params[i].def_val.s_ar[0][0])
                            p+=sprintf(forbuf+p,"'%s'",params[i].def_val.s_ar[0]);
                          else p+=sprintf(forbuf+p,"' '");
                          break;
           }
           fort_out(fp,forbuf);
           fort_out(fp,"\n");
         }
       }else if(params[i].def){
         ind=new_i_ar(params[i].size->dim);
         for(j=0;j<params[i].size->dim;j++)
           ind[j]=0;
         k=0;
         do{
           p=sprintf(forbuf,"        %s(",params[i].name->n);
           for(j=0;j<params[i].size->dim;j++){
             p+=sprintf(forbuf+p,"%d",ind[j]+1);
             if(j<params[i].size->dim-1)
               p+=sprintf(forbuf+p,",");
           }
           p+=sprintf(forbuf+p,")=");
           switch(params[i].type){
             case IVEC  : 
             case INT   : p+=sprintf(forbuf+p,"%d",params[i].def_val.i_ar[k]);
                          break;
             case FLOAT : p+=fort_out_const(forbuf+p,params[i].def_val.f_ar[k]);
                          break;
             case STRING: if(params[i].def_val.s_ar[k][0])
                            p+=sprintf(forbuf+p,"'%s'",params[i].def_val.s_ar[k]);
                          else p+=sprintf(forbuf+p,"' '");  
                          break;
           }
           fort_out(fp,forbuf);
           fort_out(fp,"\n");
           for(j=0,done=0;j<params[i].size->dim && !done;j++){
             ind[j]++;
             if(ind[j]<params[i].size->size[j]){
               done=1;
             }else{
               if(j<params[i].size->dim-1)
                 ind[j]=0;
             }
           }
           k++;
         }while(ind[params[i].size->dim-1]<params[i].size->size[params[i].size->dim-1]);
         free(ind);
       }
     }
  }

  for(i=0;i<nattribs;i++){
     sprintf(forbuf,"        set_%s=0\n",attribs[i].name->n);
     fort_out(fp,forbuf);
    if(attribs[i].def){
      for(j=0;j<attribs[i].size;j++){
        p=sprintf(forbuf,"        %s(%d)=",attribs[i].name->n,j+1);
        switch(attribs[i].type){
          case INT   : p+=sprintf(forbuf+p,"%d",attribs[i].def_val.i_ar[j]);
                       break;
          case FLOAT : p+=fort_out_const(forbuf+p,attribs[i].def_val.f_ar[j]);
                       break;
          case STRING: p+=sprintf(forbuf+p,"'%s'",attribs[i].def_val.s_ar[j]);
                       break;
        }
        fort_out(fp,forbuf);
        fort_out(fp,"\n");
      }
    }
  }
  fort_out(fp,"        return\n");
  fort_out(fp,"      end\n\n");
  
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"!  read_parameters\n");
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"      subroutine read_parameters(p_file)\n");
  fort_out(fp,"        implicit none\n\n");
  fort_out(fp,"        character*(*) p_file\n");
  fort_out(fp,"        integer ret\n");
  fort_out(fp,"        integer get_int_param, get_real_param\n");
  fort_out(fp,"        integer get_str_param, get_ivec_param\n");
  fort_out(fp,"        include 'sys_param.inc'\n");
  fort_out(fp,"        include 'gfuni0.inc'\n");
  fort_out(fp,"        include 'other_glbs.inc'\n");
  fort_out(fp,"        include 'globals.inc'\n\n");
  for(i=0;i<nparams;i++){
    if(params[i].con!=2){
      fort_out(fp,"      if(set_");
      sprintf(forbuf,"%s.ne.1) then\n",params[i].name->n);
      fort_out(fp,forbuf);
      fort_out(fp,"        ret=");
      switch(params[i].type){
        case INT   : fort_out(fp,"get_int_param");
                     break;
        case FLOAT : fort_out(fp,"get_real_param");
                     break;
        case STRING: fort_out(fp,"get_str_param");
                     break;
        case IVEC   : fort_out(fp,"get_ivec_param");
                     break;
      }
      if(params[i].type!=IVEC)
        sprintf(forbuf,"(p_file,'%s',%s,%d)\n",params[i].name->n,
                params[i].name->n,vsize(params[i].size));
      else
        sprintf(forbuf,"(p_file,'%s',%s,%d)\n",params[i].name->n,
                params[i].name->n,IVEC_SIZE);
      fort_out(fp,forbuf);
      fort_out(fp,"        if(ret.eq.1) then\n");
      fort_out(fp,"          set_");
      sprintf(forbuf,"%s=1\n",params[i].name->n);
      fort_out(fp,forbuf);
      fort_out(fp,"        end if\n");
      fort_out(fp,"      end if\n");
    }
  }
  fort_out(fp,"        return\n");
  fort_out(fp,"      end\n\n");

  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"!  sread_parameters\n");
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"      subroutine sread_parameters(p_str)\n");
  fort_out(fp,"        implicit none\n\n");
  fort_out(fp,"        character*(*) p_str\n");
  fort_out(fp,"        integer ret\n");
  fort_out(fp,"        integer sget_int_param, sget_real_param\n");
  fort_out(fp,"        integer sget_str_param, sget_ivec_param\n");
  fort_out(fp,"        include 'sys_param.inc'\n");
  fort_out(fp,"        include 'gfuni0.inc'\n");
  fort_out(fp,"        include 'other_glbs.inc'\n");
  fort_out(fp,"        include 'globals.inc'\n\n");
  for(i=0;i<nparams;i++){
    if(params[i].con!=2){
      fort_out(fp,"        if(set_");
      sprintf(forbuf,"%s.ne.1) then\n",params[i].name->n);
      fort_out(fp,forbuf);
      fort_out(fp,"          ret=");
      switch(params[i].type){
        case INT   : fort_out(fp,"sget_int_param");
                     break;
        case FLOAT : fort_out(fp,"sget_real_param");
                     break;
        case STRING: fort_out(fp,"sget_str_param");
                     break;
        case IVEC   : fort_out(fp,"sget_ivec_param");
                     break;
      }
      if(params[i].type!=IVEC)
        sprintf(forbuf,"(p_str,'%s',%s,%d)\n",params[i].name->n,
                params[i].name->n,vsize(params[i].size));
      else
        sprintf(forbuf,"(p_str,'%s',%s,%d)\n",params[i].name->n,
                params[i].name->n,IVEC_SIZE);
      fort_out(fp,forbuf);
      fort_out(fp,"          if(ret.eq.1) then\n");
      fort_out(fp,"            set_");
      sprintf(forbuf,"%s=1\n",params[i].name->n);
      fort_out(fp,forbuf);
      fort_out(fp,"          end if\n");
      fort_out(fp,"        end if\n");
    }
  }
  fort_out(fp,"        return\n");
  fort_out(fp,"      end\n\n");


  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"!  read_attributes\n");
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"      subroutine read_attributes(p_file)\n");
  fort_out(fp,"        implicit none\n\n");
  fort_out(fp,"        character*(*) p_file\n");
  fort_out(fp,"        integer ret\n");
  fort_out(fp,"        integer get_int_param, get_real_param\n");
  fort_out(fp,"        integer get_str_param, get_ivec_param\n");
  fort_out(fp,"        include 'sys_param.inc'\n");
  fort_out(fp,"        include 'gfuni0.inc'\n");
  fort_out(fp,"        include 'other_glbs.inc'\n");
  fort_out(fp,"        include 'globals.inc'\n\n");
  for(i=0;i<nattribs;i++){
    fort_out(fp,"        if(set_");
    sprintf(forbuf,"%s.ne.1) then\n",attribs[i].name->n);
    fort_out(fp,forbuf);
    fort_out(fp,"          ret=");
    switch(attribs[i].type){
      case INT   : fort_out(fp,"get_int_param");
                   break;
      case FLOAT : fort_out(fp,"get_real_param");
                   break;
      case STRING: fort_out(fp,"get_str_param");
                   break;
    }
    sprintf(forbuf,"(p_file,'%s',%s,%d)\n",attribs[i].name->n,
            attribs[i].name->n,attribs[i].size);
    fort_out(fp,forbuf);
    fort_out(fp,"          if(ret.eq.1) then\n");
    fort_out(fp,"            set_");
    sprintf(forbuf,"%s=1\n",attribs[i].name->n);
    fort_out(fp,forbuf);
    fort_out(fp,"          end if\n");
    fort_out(fp,"        end if\n");
  }
  fort_out(fp,"        return\n");
  fort_out(fp,"      end\n\n");

  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"!  check_params_attribs\n");
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"      integer function check_params_attribs()\n");
  fort_out(fp,"        implicit none\n\n");
  fort_out(fp,"        integer all_ok\n");
  fort_out(fp,"        include 'sys_param.inc'\n");
  fort_out(fp,"        include 'gfuni0.inc'\n");
  fort_out(fp,"        include 'other_glbs.inc'\n");
  fort_out(fp,"        include 'globals.inc'\n\n");
  fort_out(fp,"        all_ok=1\n");
  for(i=0;i<nparams;i++){
     if(params[i].con!=2){
        fort_out(fp,"        if(set_");
        sprintf(forbuf,"%s.eq.0 .and. (%d).eq.0) then\n",params[i].name->n,params[i].def);
        fort_out(fp,forbuf);
        fort_out(fp,"          write(*,*) 'ERROR: parameter ");
        fort_out(fp,params[i].name->n);
        fort_out(fp," has not been set.'\n");
        fort_out(fp,"          all_ok=0\n");
        fort_out(fp,"        else if(set_");
        sprintf(forbuf,"%s.eq.0 .and. (%d).eq.1) then\n",params[i].name->n,params[i].def);
        fort_out(fp,forbuf);
        fort_out(fp,"          write(*,*) 'WARNING: using default for parameter ");
        fort_out(fp,params[i].name->n);
        fort_out(fp,".'\n");
         fort_out(fp,"        end if\n");        
     }
  }
  for(i=0;i<nattribs;i++){
      fort_out(fp,"        if(set_");
      sprintf(forbuf,"%s.eq.0 .and. (%d).eq.0) then\n",attribs[i].name->n,attribs[i].def);
      fort_out(fp,forbuf);
      fort_out(fp,"          write(*,*) 'ERROR: attribute ");
      fort_out(fp,attribs[i].name->n);
      fort_out(fp," has not been set.'\n");
      fort_out(fp,"          all_ok=0\n");
      fort_out(fp,"        else if(set_");
      sprintf(forbuf,"%s.eq.0 .and. (%d).eq.1) then\n",attribs[i].name->n,attribs[i].def);
      fort_out(fp,forbuf);
      fort_out(fp,"          write(*,*) 'WARNING: using default for attribute ");
      fort_out(fp,attribs[i].name->n);
      fort_out(fp,".'\n");
      fort_out(fp,"        end if\n");        
  }
  fort_out(fp,"        check_params_attribs=all_ok\n");
  fort_out(fp,"        return\n");
  fort_out(fp,"      end\n\n");
}

void params_header(FILE *fp)
{
   int i;
   
   for(i=0;i<nparams;i++){
    if(params[i].con!=2){
      switch(params[i].type){
        case INT   : fprintf(fp,",int ");
                     break;
        case FLOAT : fprintf(fp,",double ");
                     break;
        case STRING: fprintf(fp,",char ");
                     break;
        case IVEC   : fprintf(fp,",int ");
                     break;
      }
      if(params[i].type==IVEC || params[i].type==STRING)
        fprintf(fp,"*");
      fprintf(fp,"*%s,int *set_%s",params[i].name->n,params[i].name->n);
    }
   }
}

void params_call(FILE *fp)
{
   int i;
   
   for(i=0;i<nparams;i++){
    if(params[i].con!=2){
       fprintf(fp,",");
      if(vsize(params[i].size)==1 || params[i].type==IVEC || params[i].type==STRING)
         fprintf(fp,"&");
      fprintf(fp,"%s,&set_%s",params[i].name->n,params[i].name->n);
    }
   }
}

void attribs_header(FILE *fp)
{
   int i;
   
  for(i=0;i<nattribs;i++){
     switch(attribs[i].type){
      case INT   : fprintf(fp,",int *");
                   break;
      case FLOAT : fprintf(fp,",double *");
                   break;
      case STRING: fprintf(fp,",char *");
                   break;
     }
    fprintf(fp,"%s,int *set_%s",attribs[i].name->n,attribs[i].name->n);
  }
}

void attribs_call(FILE *fp)
{
   int i;
   
  for(i=0;i<nattribs;i++){
    fprintf(fp,",%s,&set_%s",attribs[i].name->n,attribs[i].name->n);
  }
}

void check_params_header(FILE *fp)
{
   int i;
   
   for(i=0;i<nparams;i++){
    if(params[i].con!=2){
      switch(params[i].type){
        case INT   : fprintf(fp,"int ");
                     break;
        case FLOAT : fprintf(fp,"double ");
                     break;
        case STRING: fprintf(fp,"char ");
                     break;
        case IVEC   : fprintf(fp,"int ");
                     break;
      }
      if(vsize(params[i].size)>1 || params[i].type==IVEC || params[i].type==STRING)
        fprintf(fp,"*");
      fprintf(fp,"%s,int set_%s,",params[i].name->n,params[i].name->n);
    }
   }
  for(i=0;i<nattribs;i++){
     switch(attribs[i].type){
      case INT   : fprintf(fp,"int *");
                   break;
      case FLOAT : fprintf(fp,"double *");
                   break;
      case STRING: fprintf(fp,"char *");
                   break;
     }
    fprintf(fp,"%s,int set_%s",attribs[i].name->n,attribs[i].name->n);
    if(i<nattribs-1)
       fprintf(fp,",");
  }
}


void check_params_call(FILE *fp)
{
   int i;
   
   for(i=0;i<nparams;i++){
    if(params[i].con!=2){
      fprintf(fp,"%s,set_%s,",params[i].name->n,params[i].name->n);
    }
   }
   
  for(i=0;i<nattribs;i++){
    fprintf(fp,"%s,set_%s",attribs[i].name->n,attribs[i].name->n);
    if(i<nattribs-1)
       fprintf(fp,",");
  }
}

void init_params_attribs_c(FILE *fp)
{
  int i,j,l;
  
  fprintf(fp,"void read_parameters(char *p_file");
  params_header(fp);
  fprintf(fp,")\n");
  fprintf(fp,"{\n");
  for(i=0;i<nparams;i++){
    if(params[i].con!=2){
       fprintf(fp,"  if(!*set_%s)\n",params[i].name->n);
      fprintf(fp,"    if(get_param(p_file,\"%s\",",params[i].name->n);
      switch(params[i].type){
        case INT   : fprintf(fp,"\"long\",");
                     break;
        case FLOAT : fprintf(fp,"\"double\",");
                     break;
        case STRING: fprintf(fp,"\"string\",");
                     break;
        case IVEC   : fprintf(fp,"\"ivec\",");
                     break;
      }
      fprintf(fp,"%d,%s)==1)\n",vsize(params[i].size),params[i].name->n);
      fprintf(fp,"      *set_%s=1;\n",params[i].name->n);
    }
  }
  fprintf(fp,"}\n\n");

  fprintf(fp,"void sread_parameters(char *p_str");
  params_header(fp);
  fprintf(fp,")\n");
  fprintf(fp,"{\n");
  for(i=0;i<nparams;i++){
    if(params[i].con!=2){
       fprintf(fp,"  if(!*set_%s)\n",params[i].name->n);
      fprintf(fp,"    if(sget_param(p_str,\"%s\",",params[i].name->n);
      switch(params[i].type){
        case INT   : fprintf(fp,"\"long\",");
                     break;
        case FLOAT : fprintf(fp,"\"double\",");
                     break;
        case STRING: fprintf(fp,"\"string\",");
                     break;
        case IVEC  : fprintf(fp,"\"ivec\",");
                     break;
      }
      fprintf(fp,"%d,%s,1)==1)\n",vsize(params[i].size),params[i].name->n);
      fprintf(fp,"      *set_%s=1;\n",params[i].name->n);
    }
  }
  fprintf(fp,"}\n\n");

  fprintf(fp,"void read_attributes(char *p_file");
  attribs_header(fp);
  fprintf(fp,")\n");
  fprintf(fp,"{\n");
  for(i=0;i<nattribs;i++){
     fprintf(fp,"  if(!*set_%s)\n",attribs[i].name->n);
    fprintf(fp,"    if(get_param(p_file,\"%s\",",attribs[i].name->n);
    switch(attribs[i].type){
      case INT   : fprintf(fp,"\"long\",");
                   break;
      case FLOAT : fprintf(fp,"\"double\",");
                   break;
      case STRING: fprintf(fp,"\"string\",");
                   break;
    }
    fprintf(fp,"%d,%s)==1)\n",attribs[i].size,attribs[i].name->n);
    fprintf(fp,"      *set_%s=1;\n",attribs[i].name->n);
  }
  fprintf(fp,"}\n\n");
  
  fprintf(fp,"int check_params_attribs(");
  check_params_header(fp);
  fprintf(fp,")\n");
  fprintf(fp,"{\n");
  fprintf(fp,"  int all_ok=1;\n");
  for(i=0;i<nparams;i++){
    if(params[i].con!=2){
       fprintf(fp,"  if(!set_%s && !(%d)){\n",params[i].name->n,params[i].def);
       fprintf(fp,"    fprintf(stderr,\"ERROR: parameter %s has not been set.\\n\");\n",
               params[i].name->n);
       fprintf(fp,"    all_ok=0;\n");
       fprintf(fp,"  }else if(!set_%s && %d)\n",params[i].name->n,params[i].def);
       fprintf(fp,"    fprintf(stderr,\"WARNING: using default for parameter %s.\\n\");\n",
               params[i].name->n);
    }
  }
  for(i=0;i<nattribs;i++){
     fprintf(fp,"  if(!set_%s && !(%d)){\n",attribs[i].name->n,attribs[i].def);
     fprintf(fp,"    fprintf(stderr,\"ERROR: attribute %s has not been set.\\n\");\n",
             attribs[i].name->n);
     fprintf(fp,"    all_ok=0;\n");
     fprintf(fp,"  }else if(!set_%s && %d)\n",attribs[i].name->n,attribs[i].def);
     fprintf(fp,"    fprintf(stderr,\"WARNING: using default for attribute %s.\\n\");\n",
             attribs[i].name->n);
  }
  fprintf(fp,"  return(all_ok);\n");
  fprintf(fp,"}\n\n");
}

void init_coord_difs(FILE *fp)
{
  switch(language){
     case IDF :
     case UPF :
     case F77 :
     case F90 :
    case C   : init_coord_difs_c(fp);
               break;
    case ALLF: init_coord_difs_f77(fp);
               break;
  }
}

void init_coord_difs_f77(FILE *fp)
{
  int i,j,gn,nc,nc0;
  i_reg *ir;
  c_reg *cr;
  int *dn;
  
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"!  init_coord_difs\n");
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"      subroutine init_coord_difs()\n");
  fort_out(fp,"        implicit none\n\n");
  fort_out(fp,"        include 'sys_param.inc'\n");
  fort_out(fp,"        include 'gfuni0.inc'\n");
  fort_out(fp,"        include 'other_glbs.inc'\n");
  fort_out(fp,"        include 'globals.inc'\n\n");
  fort_out(fp,"10      format (a,a,i5,a)\n");
  fort_out(fp,"11      format (a,i5,a)\n");
  for(j=0;j<ncoords;j++)
    for(i=0;i<coords[j].rank-1;i++){
      sprintf(forbuf,"        if(level .gt. 0 .and. mod(%s0,2) .eq. 1) then\n",
              grid_base[j][i]->n);
      fort_out(fp,forbuf);
      sprintf(forbuf,"          write(*,11)'WARNING: %s0=',%s0,' is odd and level != 0'\n",
              grid_base[j][i]->n,grid_base[j][i]->n);
      fort_out(fp,forbuf);
      sprintf(forbuf,"          write(*,11)'WARNING: Adjusting %s0 to ',%s0-1,",
              grid_base[j][i]->n,grid_base[j][i]->n);
      fort_out(fp,forbuf);
      sprintf(forbuf,"'.  If this is part of a convergence'\n");
      fort_out(fp,forbuf);
      sprintf(forbuf,"          write(*,10)'WARNING: test, rerun level 0 calculation with'");
      fort_out(fp,forbuf);
      sprintf(forbuf,",' %s0=',%s0-1,'.'\n",grid_base[j][i]->n,grid_base[j][i]->n);
      fort_out(fp,forbuf);
      sprintf(forbuf,"          %s0 = %s0 - 1\n",grid_base[j][i]->n,grid_base[j][i]->n);
      fort_out(fp,forbuf);
      fort_out(fp,"        end if\n");
      sprintf(forbuf,"        %s = %s0 * 2**level + 1\n",grid_base[j][i]->n,grid_base[j][i]->n);
      fort_out(fp,forbuf);
    }
  dn=(int *)malloc(ncoords*sizeof(int));
  if(dn==NULL)
    fatal_error("Unable to allocate temporary storage in init_coord_difs\n");
  for(i=0;i<ncoords;dn[i++]=0);
  nc0=grids[0].crds-coords;
  for(gn=0;gn<ngrids;gn++){
    nc=grids[gn].crds-coords;
    if(dn[nc] == 0){
      dn[nc]=1;
      ir=grids[gn].reg.bounds;
      cr=grids[gn].reg.limits;
      for(i=0;i<grids[gn].reg.rank;i++,ir=ir->next,cr=cr->next){
        sprintf(forbuf,"        %s=(%s - %s)/",coord_difs[nc][i+1]->n,cr->upper->n,cr->lower->n);
        fort_out(fp,forbuf);
        fort_out(fp,"(");
        ex_to_string_f(fp,ir->upper);
        fort_out(fp," - ");
        ex_to_string_f(fp,ir->lower);
        fort_out(fp,")\n");
      }
      if(*(coord_difs[nc]) != *(coord_difs[nc0])){
        sprintf(forbuf,"        %s=lambda*sqrt((%s*%s",coord_difs[nc][0]->n,
                coord_difs[nc][1]->n,coord_difs[nc][1]->n);
        fort_out(fp,forbuf);
        for(i=2;i<coords[nc].rank;i++){
          sprintf(forbuf,"+%s*%s",coord_difs[nc][i]->n,coord_difs[nc][i]->n);
          fort_out(fp,forbuf);
        }
        sprintf(forbuf,")/%d)\n",coords[nc].rank-1);
        fort_out(fp,forbuf);
      }
    }
  }
  sprintf(forbuf,"        %s=lambda*sqrt((%s*%s",coord_difs[nc0][0]->n,
          coord_difs[nc0][1]->n,coord_difs[nc0][1]->n);
  fort_out(fp,forbuf);
  for(i=2;i<coords[nc0].rank;i++){
    sprintf(forbuf,"+%s*%s",coord_difs[nc0][i]->n,coord_difs[nc0][i]->n);
    fort_out(fp,forbuf);
  }
  sprintf(forbuf,")/%d)\n\n",coords[nc0].rank-1);
  fort_out(fp,forbuf);
  fort_out(fp,"        return\n");
  fort_out(fp,"      end\n\n");
}

void init_coord_difs_c(FILE *fp)
{
  int i,j,gn,nc,nc0;
  i_reg *ir;
  c_reg *cr;
  int *dn;
  
  for(j=0;j<ncoords;j++)
    for(i=0;i<coords[j].rank-1;i++){
      fprintf(fp,"  if(level > 0 && %s0%%2==1){\n",grid_base[j][i]->n);
      fprintf(fp,"    printf(\"WARNING: %s0=%%d is odd and level != 0\\n\",%s0);\n",
              grid_base[j][i]->n,grid_base[j][i]->n);
      fprintf(fp,"    printf(\"WARNING: Adjusting %s0 to %%d.  If this is part of a convergence\\n\",%s0-1);\n",
              grid_base[j][i]->n,grid_base[j][i]->n);
      fprintf(fp,"    printf(\"WARNING: test, rerun level 0 calculation with %s0 = %%d.\\n\",%s0-1);\n",
              grid_base[j][i]->n,grid_base[j][i]->n);
      fprintf(fp,"    %s0-=1;\n",grid_base[j][i]->n);
      fprintf(fp,"  }\n");
      fprintf(fp,"  %s=%s0*(int)pow(2.0,(double)level)+1;\n",
              grid_base[j][i]->n,grid_base[j][i]->n);
    }
  dn=(int *)malloc(ncoords*sizeof(int));
  if(dn==NULL)
    fatal_error("Unable to allocate temporary storage in init_coord_difs\n");
  for(i=0;i<ncoords;dn[i++]=0);
  nc0=grids[0].crds-coords;
  for(gn=0;gn<ngrids;gn++){
    nc=grids[gn].crds-coords;
    if(dn[nc] == 0){
      dn[nc]=1;
      ir=grids[gn].reg.bounds;
      cr=grids[gn].reg.limits;
      for(i=0;i<grids[gn].reg.rank;i++,ir=ir->next,cr=cr->next){
        fprintf(fp,"  %s=(%s - %s)/(",coord_difs[nc][i+1]->n,cr->upper->n,cr->lower->n);
        ex_to_string(fp,ir->upper);
        fprintf(fp," - ");
        ex_to_string(fp,ir->lower);
        fprintf(fp,");\n");
      }
      if(*(coord_difs[nc]) != *(coord_difs[nc0])){
        fprintf(fp,"  %s=lambda*sqrt((%s*%s",coord_difs[nc][0]->n,
                (*(coord_difs[nc]+1))->n,coord_difs[nc][1]->n);
        for(i=2;i<coords[nc].rank;i++)
          fprintf(fp,"+%s*%s",coord_difs[nc][i]->n,coord_difs[nc][i]->n);
        fprintf(fp,")/%d);\n",coords[nc].rank-1);
      }
    }
  }
  fprintf(fp,"  %s=lambda*sqrt((%s*%s",coord_difs[nc0][0]->n,
          (*(coord_difs[nc0]+1))->n,coord_difs[nc0][1]->n);
  for(i=2;i<coords[nc0].rank;i++)
    fprintf(fp,"+%s*%s",coord_difs[nc0][i]->n,coord_difs[nc0][i]->n);
  fprintf(fp,")/%d);\n",coords[nc0].rank-1);
  fprintf(fp,"\n");
}

void init_grids(FILE *fp)
{
  switch(language){
     case IDF :
     case UPF :
     case F77 :
     case F90 :
    case C   : init_grids_c(fp);
               break;
    case ALLF: init_grids_f77(fp);
               break;
  }
}

void init_grids_f77(FILE *fp)
{
  int gn,r,i,j,fn,nflv,cn;
  i_reg *ir;
  c_reg *cr;
  coord_list *cd,*fcd;
  int d,bd;
  int **t;
  
  if((t=(int **)malloc(sizeof(int *)*ncoords))==NULL)
    fatal_error("Can't malloc temp space in init_grids.");
  for(r=0;r<ncoords;r++){
    if((t[r]=(int *)malloc(sizeof(int)*coords[r].rank))==NULL)
      fatal_error("Can't malloc temp space in init_grids.");
    for(d=0;d<coords[r].rank;d++)
      t[r][d]=0;
  }
  
   nflv=get_size_all();
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"!  init_lats\n");
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"      subroutine init_lats()\n");
  fort_out(fp,"        implicit none\n\n");
  fort_out(fp,"        include 'sys_param.inc'\n");
  fort_out(fp,"        include 'gfuni0.inc'\n");
  fort_out(fp,"        include 'other_glbs.inc'\n");
  fort_out(fp,"        include 'globals.inc'\n\n");
  fort_out(fp,"        integer i,mmaloc\n\n");
  for(gn=0;gn<ncoords;gn++){
    cd=coords[gn].c_names->next;
    if(cd==NULL){
      fatal_error("init_lats: no spatial coordinates\n");
    }
    i=sprintf(forbuf,"        cname(%d)='%s",gn+1,cd->name->n);
    for(r=2,cd=cd->next;r<coords[gn].rank;r++,cd=cd->next){
      i+=sprintf(forbuf+i,"|%s",cd->name->n);
    }
    sprintf(forbuf+i,"'\n");
    fort_out(fp,forbuf);
  }
  sprintf(forbuf,"        ncnames = %d\n",(MAXRANK+1)*ncoords);
  fort_out(fp,forbuf);
  sprintf(forbuf,"        ngfcn = %d\n",nflv);
  fort_out(fp,forbuf);
  sprintf(forbuf,"        nptrs = sizelatcb * %d + 2 * ngfcn\n",ngrids);
  fort_out(fp,forbuf);
  i=2*nflv+1;
  for(gn=0;gn<ngrids;gn++){
    cn=grids[gn].crds - coords;
    sprintf(forbuf,"        ptrs(%d)=%d\n",i++,gn+1); /* id is set to lattice number */
    fort_out(fp,forbuf);
    sprintf(forbuf,"        ptrs(%d)=%d\n",i++,cn+1); /* cs is set to coordinate system number */
    fort_out(fp,forbuf);
    sprintf(forbuf,"        ptrs(%d)=%d\n",i++,grids[gn].reg.rank); /* set rank */
    fort_out(fp,forbuf);
    for(j=i,ir=grids[gn].reg.bounds;j<i+grids[gn].reg.rank;j++,ir=ir->next){
      sprintf(forbuf,"        ptrs(%d)=",j);
      fort_out(fp,forbuf);
      ex_to_string_f(fp,ir->upper);
      fort_out(fp,"-");
      ex_to_string_f(fp,ir->lower);
      fort_out(fp,"+1\n");
    }
    i+=MAXRANK;
    for(;j<i;j++){
      sprintf(forbuf,"        ptrs(%d)=1\n",j);
      fort_out(fp,forbuf);
    }
    for(j=i,ir=grids[gn].reg.bounds;j<i+2*grids[gn].reg.rank;j+=2,ir=ir->next){
      sprintf(forbuf,"        ptrs(%d)=",j);
      fort_out(fp,forbuf);
      ex_to_string_f(fp,ir->lower);
      fort_out(fp,"\n");
      sprintf(forbuf,"        ptrs(%d)=",j+1);
      fort_out(fp,forbuf);
      ex_to_string_f(fp,ir->upper);
      fort_out(fp,"\n");
    }
    i+=2*MAXRANK;
    for(;j<i;j++){
      sprintf(forbuf,"        ptrs(%d)=1\n",j);
      fort_out(fp,forbuf);
    }
    for(fcd=cd=grids[gn].clst,cr=grids[gn].reg.limits,r=0;r<grids[gn].reg.rank;
        j++,r++,cr=cr->next,cd=cd->next){
      if(!t[cn][r+1]){
        t[cn][r+1]=1;
        if(r==0){
          sprintf(forbuf,"        %s=mmaloc(getsize(%d))\n",cd->name->n,gn+1);
          fort_out(fp,forbuf);
          sprintf(forbuf,"        if(%s .eq. -1) then\n",cd->name->n);
          fort_out(fp,forbuf);
          fort_out(fp,"          write(*,*)'ERROR: unable to allocate coordinate'\n");
          fort_out(fp,"          write(*,*)'increase memsiz and recompile'\n");
          fort_out(fp,"          stop\n");
          fort_out(fp,"        end if\n");
        }else{
          sprintf(forbuf,"        %s=%s+getshape(%d,%d)\n",cd->name->n,fcd->name->n,gn+1,r);
          fort_out(fp,forbuf);
          fcd=fcd->next;
        }
        fort_out(fp,"        call rdvramp(");
        sprintf(forbuf,"q(%s),getshape(%d,%d)",cd->name->n,gn+1,r+1);
        fort_out(fp,forbuf);
        sprintf(forbuf,",%s,%s)\n",cr->lower->n,coord_difs[cn][rank(cd->name)+1]->n);
        fort_out(fp,forbuf);
      }
      sprintf(forbuf,"        ptrs(%d)=%s\n",j,cd->name->n);
      fort_out(fp,forbuf);
    }
    i+=MAXRANK;
  }
  fort_out(fp,"        return\n");
  fort_out(fp,"      end\n\n");

  for(r=0;r<ncoords;r++)
    if(t[r])
      free(t[r]);
  if(t)
    free(t);
}

void init_grids_c(FILE *fp)
{
  int gn,r,cn,d,bd,cnl;
  i_reg *ir;
  c_reg *cr;
  coord_list *cd;
  int **t;
  
  if((t=(int **)malloc(sizeof(int *)*ncoords))==NULL)
    fatal_error("Can't malloc temp space in init_grids.");
  for(r=0;r<ncoords;r++){
    if((t[r]=(int *)malloc(sizeof(int)*coords[r].rank))==NULL)
      fatal_error("Can't malloc temp space in init_grids.");
    for(d=0;d<coords[r].rank;d++)
      t[r][d]=0;
  }
    
  fprintf(fp,"  lats=(lattice_type *)malloc(%d*sizeof(lattice_type));\n",ngrids);
  fprintf(fp,"  if(lats==NULL){\n");
  fprintf(fp,"    printf(\"Unable to malloc lats.\\n\");\n");
  fprintf(fp,"    exit(1);\n");
  fprintf(fp,"  }\n");
  fprintf(fp,"  cname=(char **)malloc(%d*sizeof(char *));\n",ncoords);
  fprintf(fp,"  if(cname==NULL){\n");
  fprintf(fp,"    printf(\"Unable to malloc cname.\\n\");\n");
  fprintf(fp,"    exit(1);\n");
  fprintf(fp,"  }\n");
  for(gn=0;gn<ncoords;gn++){
    for(cnl=0,r=1,cd=coords[gn].c_names->next;r<coords[gn].rank;r++,cd=cd->next){
      cnl+=strlen(cd->name->n)+1;
    }
    fprintf(fp,"  cname[%d]=(char *)malloc(%d*sizeof(char));\n",gn,cnl);
    fprintf(fp,"  if(cname[%d]==NULL){\n",gn);
    fprintf(fp,"    printf(\"Unable to malloc cname[%d].\\n\");\n",gn);
    fprintf(fp,"    exit(1);\n");
    fprintf(fp,"  }\n");
    cd=coords[gn].c_names->next;
    fprintf(fp,"  strcpy(cname[%d],\"%s",gn,cd->name->n);            
    for(r=2,cd=cd->next;r<coords[gn].rank;r++,cd=cd->next){
      fprintf(fp,"|%s",cd->name->n);            
    }
    fprintf(fp,"\");\n");
  }
  for(gn=0;gn<ngrids;gn++){
    cn=grids[gn].crds - coords;
    for(bd=0,cd=grids[gn].clst,r=0,ir=grids[gn].reg.bounds,cr=grids[gn].reg.limits;
        r<grids[gn].reg.rank;r++,ir=ir->next,cr=cr->next,cd=cd->next){
      fprintf(fp,"  %s_%d.n_coord=(",grids[gn].name->n,r);
      ex_to_string(fp,ir->upper);
      fprintf(fp,"-");
      ex_to_string(fp,ir->lower);
      fprintf(fp,"+1);\n");
      if(!t[cn][r+1]){
        t[cn][r+1]=1;
        fprintf(fp,"  %s=vec_alloc_n(%s_%d.n_coord,\"%s\");\n",cd->name->n,
                grids[gn].name->n,r,cd->name->n);
        fprintf(fp,"  rdvramp(%s,%s_%d.n_coord",cd->name->n,grids[gn].name->n,r);
        fprintf(fp,",%s,%s);\n",cr->lower->n,coord_difs[cn][rank(cd->name)+1]->n);
      }
      fprintf(fp,"  %s_%d.coord=%s;\n",grids[gn].name->n,r,cd->name->n);
      fprintf(fp,"  lats[%d].shape[%d]=%s_%d.n_coord;\n",gn,r,
              grids[gn].name->n,r);
      fprintf(fp,"  lats[%d].bounds[%d]=",gn,bd++);
      ex_to_string(fp,ir->lower);
      fprintf(fp,";\n  lats[%d].bounds[%d]=",gn,bd++);
      ex_to_string(fp,ir->upper);
      fprintf(fp,";\n");
    }
    fprintf(fp,"  lats[%d].coords=vec_alloc_n(",gn);
    for(r=0;r<grids[gn].reg.rank;r++){
      fprintf(fp,"lats[%d].shape[%d]",gn,r);
      if(r+1 < grids[gn].reg.rank)
        fprintf(fp,"*");
    }
    fprintf(fp,",\"lats[%d].coords\");\n",gn);
    fprintf(fp,"  lc=lats[%d].coords;\n",gn);
    fprintf(fp,"  rdvcpy(lc,%s_0.coord,lats[%d].shape[0]);\n",grids[gn].name->n,
            gn);
    for(r=1;r<grids[gn].reg.rank;r++){
      fprintf(fp,"  lc+=lats[%d].shape[%d];\n",gn,r-1);
      fprintf(fp,"  rdvcpy(lc,%s_%d.coord,lats[%d].shape[%d]);\n",
              grids[gn].name->n,r,gn,r);
    }
    fprintf(fp,"  lats[%d].rank=%d;\n",gn,grids[gn].reg.rank);
    fprintf(fp,"  lats[%d].cs=%d;\n",gn,cn);
  }
  fprintf(fp,"\n");

  for(r=0;r<ncoords;r++)
    if(t[r])
      free(t[r]);
  if(t)
    free(t);
}

void init_gfuncs(FILE *fp)
{
  switch(language){
     case IDF :
     case UPF :
     case F77 :
     case F90 :
    case C   : init_gfuncs_c(fp);
               break;
    case ALLF: init_gfuncs_f77(fp);
               break;
  }
}

void init_gfuncs_f77(FILE *fp)
{
  int fn,i,j,nflv,n,st,nrev,ne;
  offset_type *o;
  char *d;
  work_list *wl;

  for(nrev=0,fn=0,nflv=0;fn<ngfuncs;fn++){
    nflv+=gfuncs[fn].ntlevs;
    if((i=resid_exists(gfuncs[fn].fname))!=-1 && resids[i].eval){
      nflv++;
      nrev++;
    }
  }

  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"!  init_gfuncs\n");
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"      subroutine init_gfuncs()\n");
  fort_out(fp,"        implicit none\n\n");
  fort_out(fp,"        integer  mmaloc\n");
  sprintf(forbuf,"        character*(%d)  temp\n",STR_P_SIZE*2);
  fort_out(fp,forbuf);
  fort_out(fp,"        character*256  catsqz\n");
  fort_out(fp,"        include 'sys_param.inc'\n");
  fort_out(fp,"        include 'gfuni0.inc'\n");
  fort_out(fp,"        include 'other_glbs.inc'\n");
  fort_out(fp,"        include 'globals.inc'\n\n");
  fort_out(fp,"901     format (a,i1)\n");
  fort_out(fp,"902     format (a,i1,a)\n");
  for(wl=static_work;wl!=NULL;wl=wl->next){
    sprintf(forbuf,"        nwork%d=",wl->work->num);
    fort_out(fp,forbuf);
    ex_to_string_f(fp,wl->work->expr);
    fort_out(fp,"\n");
    sprintf(forbuf,"        work%d=mmaloc(nwork%d)\n",wl->work->num,
            wl->work->num);
    fort_out(fp,forbuf);
    sprintf(forbuf,"        if(work%d .eq. -1) then\n",wl->work->num);
    fort_out(fp,forbuf);
    fort_out(fp,"          write(*,*)'ERROR: unable to allocate work array'\n");
    fort_out(fp,"          write(*,*)'increase memsiz and recompile'\n");
    fort_out(fp,"          stop\n");
    fort_out(fp,"        end if\n");
  }
  for(st=0,n=0,ne=0,fn=0;fn<ngfuncs;fn++){
    d=(gfuncs[fn].desc==NULL)?gfuncs[fn].fname->n:gfuncs[fn].desc;
    for(o=gfuncs[fn].tlev;o!=NULL;o=o->next,n++){
      i=gptr_to_index(gfuncs[fn].grd);
      sprintf(forbuf,"        ptrs(ngfcn + %d) = 2*ngfcn + %d*sizelatcb + 1\n",n+1,i);
      fort_out(fp,forbuf);
      if(gfuncs[fn].alias && gfuncs[fn].ntlevs>1 && o->next==NULL){
        sprintf(forbuf,"        ptrs(%d) = ptrs(%d)\n",n+1,n+2-gfuncs[fn].ntlevs);
        fort_out(fp,forbuf);
      }else{
        sprintf(forbuf,"        ptrs(%d) = mmaloc(getsize(%d))\n",n+1,i+1);
        fort_out(fp,forbuf);
        sprintf(forbuf,"        if(ptrs(%d) .eq. -1) then\n",n+1);
        fort_out(fp,forbuf);
        fort_out(fp,"          write(*,*)'ERROR: unable to allocate grid function'\n");
        fort_out(fp,"          write(*,*)'increase memsiz and recompile'\n");
        fort_out(fp,"          stop\n");
        fort_out(fp,"        end if\n");
      }
    }
    sprintf(forbuf,"        write(temp,901) '%s_',level\n",d);
    fort_out(fp,forbuf);
    sprintf(forbuf,"        attrfname(%d) = catsqz(tag,temp)\n",ne+1);
    fort_out(fp,forbuf);
    sprintf(forbuf,"        gf_ln(%d)=%d\n",ne+1,gfuncs[fn].ntlevs);
    fort_out(fp,forbuf);
    sprintf(forbuf,"        gf_st(%d)=%d\n",ne+1,st);
    fort_out(fp,forbuf);
    ne++;
    st+=gfuncs[fn].ntlevs;
  }
  for(i=0;i<nresids;){
    if(resids[i].eval){
      fn=gptr_to_index(resids[i].gfunc->grd);
      sprintf(forbuf,"        ptrs(ngfcn + %d) = 2*ngfcn + %d*sizelatcb + 1\n",n+1,fn);
      fort_out(fp,forbuf);
      sprintf(forbuf,"        ptrs(%d) = mmaloc(getsize(%d))\n",n+1,fn+1);
      fort_out(fp,forbuf);
      sprintf(forbuf,"        if(ptrs(%d) .eq. -1) then\n",n+1);
      fort_out(fp,forbuf);
      fort_out(fp,"          write(*,*)'ERROR: unable to allocate residual'\n");
      fort_out(fp,"          write(*,*)'increase memsiz and recompile'\n");
      fort_out(fp,"          stop\n");
      fort_out(fp,"        end if\n");
      sprintf(forbuf,"        write(temp,901) '%s_res_',level\n",
              resids[i].gfunc->fname->n);
      fort_out(fp,forbuf);
      sprintf(forbuf,"        attrfname(%d) = catsqz(tag,temp)\n",ne+1);
      fort_out(fp,forbuf);
      sprintf(forbuf,"        gf_ln(%d)=1\n",ne+1);
      fort_out(fp,forbuf);
      sprintf(forbuf,"        gf_st(%d)=%d\n",ne+1,st);
      fort_out(fp,forbuf);
      n++;
      ne++;
      st++;
    }
    for(i++;i<nresids && resids[i].gfunc==resids[i-1].gfunc;i++);
  }
  fort_out(fp,"        return\n");
  fort_out(fp,"      end\n\n");
}

void init_gfuncs_c(FILE *fp)
{
  int fn,i,j,nflv,n,st,nrev,ne;
  offset_type *o;
  char *d;
  work_list *wl;

  for(nrev=0,fn=0,nflv=0;fn<ngfuncs;fn++){
    nflv+=gfuncs[fn].ntlevs;
    if((i=resid_exists(gfuncs[fn].fname))!=-1 && resids[i].eval){
      nflv++;
      nrev++;
    }
  }

  fprintf(fp,"  fname=(char **)malloc(%d*sizeof(char *));\n",ngfuncs+nrev);
  for(wl=static_work;wl!=NULL;wl=wl->next){
    fprintf(fp,"  nwork%d=",wl->work->num);
    ex_to_string(fp,wl->work->expr);
    fprintf(fp,";\n");
    fprintf(fp,"  work%d=vec_alloc_n(nwork%d,\"work%d\");\n",wl->work->num,
            wl->work->num,wl->work->num);
  }
  for(st=0,n=0,fn=0,ne=0;fn<ngfuncs;fn++){
    switch(gfuncs[fn].type){
      case INT   :  for(o=gfuncs[fn].tlev;o!=NULL;o=o->next,n++){
                      if(gfuncs[fn].alias && gfuncs[fn].ntlevs>1 && o->next==NULL){
                        fprintf(fp,"  %s",gfuncs[fn].fname->n);
                        gfunc_suffix(fp,o->offset);
                        fprintf(fp,"=%s",gfuncs[fn].fname->n);
                        gfunc_suffix(fp,gfuncs[fn].tlev->offset);
                        fprintf(fp,";\n");
                      }else{
                        fprintf(fp,"  %s",gfuncs[fn].fname->n);
                        gfunc_suffix(fp,o->offset);
                        fprintf(fp,"=ivec_alloc_n(");
                        grid_size_r(fp,gfuncs[fn].grd);
                        fprintf(fp,",\"%s",gfuncs[fn].fname->n);
                        gfunc_suffix(fp,o->offset);
                        fprintf(fp,"\");\n");
                      }
                    }
                    break;
      case FLOAT :  for(o=gfuncs[fn].tlev;o!=NULL;o=o->next,n++){
                      if(gfuncs[fn].alias && gfuncs[fn].ntlevs>1 && o->next==NULL){
                        fprintf(fp,"  %s",gfuncs[fn].fname->n);
                        gfunc_suffix(fp,o->offset);
                        fprintf(fp,"=%s",gfuncs[fn].fname->n);
                        gfunc_suffix(fp,gfuncs[fn].tlev->offset);
                        fprintf(fp,";\n");
                      }else{
                        fprintf(fp,"  %s",gfuncs[fn].fname->n);
                        gfunc_suffix(fp,o->offset);
                        fprintf(fp,"=vec_alloc_n(");
                        grid_size_r(fp,gfuncs[fn].grd);
                        fprintf(fp,",\"%s",gfuncs[fn].fname->n);
                        gfunc_suffix(fp,o->offset);
                        fprintf(fp,"\");\n");
                      }
                      }
                      break;
    }
    d=(gfuncs[fn].desc==NULL)?gfuncs[fn].fname->n:gfuncs[fn].desc;
    fprintf(fp,"  fname[%d]=(char *)malloc(%d*sizeof(char));\n",ne,strlen(d)+13);
    fprintf(fp,"  sprintf(fname[%d],\"%%s%s_%%d\",tag,level);\n",ne,d);
    st+=gfuncs[fn].ntlevs;
    ne++;
  }
  for(i=0;i<nresids;){
    if(resids[i].eval)
      switch(resids[i].gfunc->type){
        case INT  :  fprintf(fp,"  %s_res=ivec_alloc_n(",resids[i].gfunc->fname->n);
                    grid_size_r(fp,resids[i].gfunc->grd);
                    fprintf(fp,",\"%s_res\");\n",resids[i].gfunc->fname->n);
                    fprintf(fp,"  fname[%d]=(char *)malloc(%d*sizeof(char));\n",ne,
                            strlen(resids[i].gfunc->fname->n)+17);
                    fprintf(fp,"  sprintf(fname[%d],\"%%s%s_res_%%d\",tag,level);\n",ne,
                            resids[i].gfunc->fname->n);
                    st++;
                    n++;ne++;
                    break;
        case FLOAT: fprintf(fp,"  %s_res=vec_alloc_n(",resids[i].gfunc->fname->n);
                     grid_size_r(fp,resids[i].gfunc->grd);
                     fprintf(fp,",\"%s_res\");\n",resids[i].gfunc->fname->n);
                    fprintf(fp,"  fname[%d]=(char *)malloc(%d*sizeof(char));\n",ne,
                            strlen(resids[i].gfunc->fname->n)+17);
                    fprintf(fp,"  sprintf(fname[%d],\"%%s%s_res_%%d\",tag,level);\n",ne,
                            resids[i].gfunc->fname->n);
                    st++;
                    n++;ne++;
                    break;
      }
    for(i++;i<nresids && resids[i].gfunc==resids[i-1].gfunc;i++);
  }
  fprintf(fp,"\n");
}

void read_state(FILE *fp,char *iname)
{
  switch(language){
     case IDF :
     case UPF :
     case F77 :
     case F90 :
    case C   : read_state_c(fp,iname);
               break;
    case ALLF: read_state_f77(fp,iname);
               break;
  }
}

void read_state_f77(FILE *fp, char *iname)
{
  int fn,i,j,k,nflv,n;
  char *d;

  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"!  read_state\n");
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"      subroutine read_state(argc,argv)\n");
  fort_out(fp,"        implicit none\n\n");
  fort_out(fp,"        include 'sys_param.inc'\n");
  fort_out(fp,"        include 'gfuni0.inc'\n");
  fort_out(fp,"        include 'globals.inc'\n\n");
  fort_out(fp,"        integer argc,i\n");
  fort_out(fp,"        character*64 argv(argc)\n");
  fort_out(fp,"        character*256 command\n");
  fort_out(fp,"        integer getu,ret\n");
  fort_out(fp,"        integer fp, indlnb, gft_read_id_gf\n\n");
  fort_out(fp,"        fp=getu()\n");
  fort_out(fp,"        open(unit=fp,file=in_file(1:indlnb(in_file)),\n");
  fort_out(fp,"     &    status = 'old', form = 'unformatted', iostat = ret)\n");
  fort_out(fp,"        if(ret .ne. 0) then\n");
  fort_out(fp,"          write (*,*) 'Can''t open ',in_file\n");
  fort_out(fp,"          write (*,*) 'Calling initial data generator.'\n");
  if(parallel==TRIV){
    fort_out(fp,"          open(unit=fp,file=");
    sprintf(forbuf,"'%s',",iname);
    fort_out(fp,forbuf);
    fort_out(fp,"status = 'old',");
    fort_out(fp,"form = 'unformatted',");
    fort_out(fp,"iostat = ret)\n");
    fort_out(fp,"          if(ret .eq. 0) then\n");
    fort_out(fp,"            close(fp)\n");
    sprintf(forbuf,"            command='%s'\n",iname);
    fort_out(fp,forbuf);
    fort_out(fp,"          else\n");
    sprintf(forbuf,"            command='../%s'\n",iname);
    fort_out(fp,forbuf);
    fort_out(fp,"          end if\n");
  }else if(parallel==0){
     sprintf(forbuf,"          command='%s'\n",iname);
     fort_out(fp,forbuf);
  }
  fort_out(fp,"          do i=1,argc\n");
  fort_out(fp,"            command=command(1:indlnb(command))");
  fort_out(fp,"//' '//argv(i)\n");
  fort_out(fp,"          end do\n");
  fort_out(fp,"          call system(command)\n");
  fort_out(fp,"          open(unit=fp,file=in_file(1:indlnb(in_file)),\n");
  fort_out(fp,"     &      status = 'old', form = 'unformatted', iostat = ret)\n");
  fort_out(fp,"          if(ret .ne. 0) then\n");
  fort_out(fp,"            write (*,*) 'Can''t open ',in_file\n");
  fort_out(fp,"            write (*,*) 'Assuming updates will do initialization'\n");
  fort_out(fp,"            return\n");
  fort_out(fp,"          else\n");
  fort_out(fp,"            close(fp)\n");
  fort_out(fp,"          end if\n");
  fort_out(fp,"        else\n");
  fort_out(fp,"          close(fp)\n");
  fort_out(fp,"        end if\n");
 
  fort_out(fp,"        call gft_set_single(in_file(1:indlnb(in_file)))\n");
  for(fn=0;fn<ngfuncs;fn++){
    if(gfuncs[fn].ntlevs==1){
      j=0;
      i=0;
    }else{
      j=gfuncs[fn].ntlevs-2;
      i=1;
    }
    while(j>=0){
      fort_out(fp,"        ret=gft_read_id_gf(");
      sprintf(forbuf,"'%s[%d]',ptrs(getpshape(%d)),",gfuncs[fn].fname->n,j,
              gptr_to_index(gfuncs[fn].grd)+1);
      fort_out(fp,forbuf);
      sprintf(forbuf,"getrank(%d),q(ptrs(gf_st(%d)+%d)))\n",gptr_to_index(gfuncs[fn].grd)+1,
              fn+1,i+1);
      fort_out(fp,forbuf);
      i++;
      j--;
    }
  }
  fort_out(fp,"        call read_params_attribs()\n");
  fort_out(fp,"        call gft_set_multi()\n");
  fort_out(fp,"        return\n");
  fort_out(fp,"      end\n\n");
}

void read_state_header(FILE *fp)
{
   int fn,i;
   offset_type *o;
   
  fprintf(fp,"void read_state(int argc, char **argv, lattice_type *lats");
  for(fn=0;fn<ngfuncs;fn++){
    if(gfuncs[fn].ntlevs==1){
      o=gfuncs[fn].tlev;
    }else{
      o=gfuncs[fn].tlev->next;
    }
    while(o!=NULL){
       switch(gfuncs[fn].type){
          case INT:
             fprintf(fp,",int *");
             break;
          case FLOAT:
             fprintf(fp,",double *");
             break;
       }
      fprintf(fp,"%s",gfuncs[fn].fname->n);
      gfunc_suffix(fp,o->offset);
      o=o->next;
    }
  }
  
   for(i=0;i<nparams;i++){
    if(params[i].con!=2){
      switch(params[i].type){
        case INT   : fprintf(fp,",int ");
                     break;
        case FLOAT : fprintf(fp,",double ");
                     break;
        case STRING: fprintf(fp,",char ");
                     break;
        case IVEC   : fprintf(fp,",int ");
                     break;
      }
      if(params[i].type==STRING)
        fprintf(fp,"*");
      fprintf(fp,"*%s,int set_%s",params[i].name->n,params[i].name->n);
    }
   }
  fprintf(fp,")\n");
}


void read_state_call(FILE *fp)
{
   int fn,i;
   offset_type *o;
   
  fprintf(fp,"read_state(argc, argv, lats");
  for(fn=0;fn<ngfuncs;fn++){
    if(gfuncs[fn].ntlevs==1){
      o=gfuncs[fn].tlev;
    }else{
      o=gfuncs[fn].tlev->next;
    }
    while(o!=NULL){
      fprintf(fp,",%s",gfuncs[fn].fname->n);
      gfunc_suffix(fp,o->offset);
      o=o->next;
    }
  }
   for(i=0;i<nparams;i++){
    if(params[i].con!=2){
       fprintf(fp,",");
      if(vsize(params[i].size)==1 || params[i].type==STRING)
         fprintf(fp,"&");
      fprintf(fp,"%s,set_%s",params[i].name->n,params[i].name->n);
    }
   }
  fprintf(fp,");\n");
}

void read_state_c(FILE *fp,char *iname)
{
  int fn,i;
  offset_type *o;

   read_state_header(fp);
  fprintf(fp,"{\n");
  fprintf(fp,"  char command[256];\n");
  fprintf(fp,"  FILE *fp;\n");
  fprintf(fp,"  int res,i,j;\n\n");
  fprintf(fp,"  fp=fopen(*in_file,\"r\");\n");
  fprintf(fp,"  if(fp==NULL){\n");
  fprintf(fp,"    fprintf(stderr,\"Can't open %%s.\\n\",*in_file);\n");
  fprintf(fp,"    fprintf(stderr,\"Calling initial data generator.\\n\");\n");
  if(parallel==TRIV){
     fprintf(fp,"    if(fp=fopen(\"%s\",\"r\")){\n",iname);
     fprintf(fp,"      fclose(fp);\n");
     fprintf(fp,"      j=sprintf(command,\"%s\");\n",iname);
     fprintf(fp,"    }else\n");
     fprintf(fp,"      j=sprintf(command,\"../%s\");\n",iname);
  }else if(parallel==0){
     fprintf(fp,"    j=sprintf(command,\"%s\");\n",iname);
  }
  fprintf(fp,"    for(i=1;i<argc;i++)\n");
  fprintf(fp,"      j+=sprintf(command+j,\" %%s\",argv[i]);\n");
  fprintf(fp,"    system(command);\n");
  fprintf(fp,"    fp=fopen(*in_file,\"r\");\n");
  fprintf(fp,"    if(fp==NULL){\n");
  fprintf(fp,"      fprintf(stderr,\"Can't open %%s.\\n\",*in_file);\n");
  fprintf(fp,"      fprintf(stderr,\"Assuming updates will take care of initialization.\\n\");\n");
  fprintf(fp,"      return;\n");
  fprintf(fp,"    }else fclose(fp);\n");
  fprintf(fp,"  }else fclose(fp);\n");
 
  fprintf(fp,"  gft_set_single(*in_file);\n");
  for(fn=0;fn<ngfuncs;fn++){
    if(gfuncs[fn].ntlevs==1){
      o=gfuncs[fn].tlev;
      i=0;
    }else{
      o=gfuncs[fn].tlev->next;
      i=gfuncs[fn].ntlevs-2;
    }
    while(o!=NULL){
      fprintf(fp,"  gft_read_id_gf(\"%s\",lats[%d].shape,&lats[%d].rank,%s",
              gfuncs[fn].fname->n,gptr_to_index(gfuncs[fn].grd),gptr_to_index(gfuncs[fn].grd),
              gfuncs[fn].fname->n);
      gfunc_suffix(fp,o->offset);
      fprintf(fp,");\n");
      i--;
      o=o->next;
    }
  }
  for(i=0;i<nparams;i++){
    if(params[i].con!=2){
       fprintf(fp,"  if(!set_%s)\n",params[i].name->n);
      switch(params[i].type){
        case IVEC   :
        case INT   : fprintf(fp,"    gft_read_id_int(");
                     break;
        case FLOAT : fprintf(fp,"    gft_read_id_float(");
                     break;
        case STRING: fprintf(fp,"    gft_read_id_str(");
                     break;
      }
      fprintf(fp,"\"%s\",",params[i].name->n);
      fprintf(fp,"%s,%d);\n",params[i].name->n,vsize(params[i].size));
    }
  }
  fprintf(fp,"  gft_set_multi();\n");
  fprintf(fp,"}\n\n");
}

void read_params_attribs(FILE *fp)
{
  switch(language){
     case IDF :
     case UPF :
     case F77 :
     case F90 :
    case C   : 
               break;
    case ALLF: read_params_attribs_f77(fp);
               break;
  }
}

void read_params_attribs_f77(FILE *fp)
{
  int i;

  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"!  read_params_attribs\n");
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"      subroutine read_params_attribs()\n");
  fort_out(fp,"        implicit none\n\n");
  fort_out(fp,"        include 'sys_param.inc'\n");
  fort_out(fp,"        include 'gfuni0.inc'\n");
  fort_out(fp,"        include 'globals.inc'\n");
  fort_out(fp,"        include 'other_glbs.inc'\n");
  fort_out(fp,"        integer res, gft_read_id_int\n");
  fort_out(fp,"        integer gft_read_id_float, gft_read_id_str\n\n");
  for(i=0;i<nparams;i++){
    if(params[i].con!=2){
      sprintf(forbuf,"        if(set_%s.eq.0) then\n",params[i].name->n);
      fort_out(fp,forbuf);
      switch(params[i].type){
        case IVEC  :
        case INT   : fort_out(fp,"          res=gft_read_id_int(");
                     break;
        case FLOAT : fort_out(fp,"          res=gft_read_id_float(");
                     break;
        case STRING: fort_out(fp,"          res=gft_read_id_str(");
                     break;
      }
      sprintf(forbuf,"'%s',",params[i].name->n);
      fort_out(fp,forbuf);
      sprintf(forbuf,"%s,%d)\n",params[i].name->n,vsize(params[i].size));
      fort_out(fp,forbuf);
      fort_out(fp,"        end if\n");
    }
  }
  fort_out(fp,"        return\n");
  fort_out(fp,"      end\n\n");
}

void write_params_attribs(FILE *fp)
{
  switch(language){
     case IDF :
     case UPF :
     case F77 :
     case F90 :
    case C   : 
               break;
    case ALLF: write_params_attribs_f77(fp);
               break;
  }
}

void write_params_attribs_f77(FILE *fp)
{
  int i;

  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"!  write_params_attribs\n");
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"      subroutine write_params_attribs()\n");
  fort_out(fp,"        implicit none\n\n");
  fort_out(fp,"        include 'sys_param.inc'\n");
  fort_out(fp,"        include 'gfuni0.inc'\n");
  fort_out(fp,"        include 'globals.inc'\n");
  fort_out(fp,"        include 'other_glbs.inc'\n");
  fort_out(fp,"        integer res, gft_write_id_int\n");
  fort_out(fp,"        integer gft_write_id_float, gft_write_id_str\n\n");
  for(i=0;i<nparams;i++){
    if(params[i].con!=2){
      switch(params[i].type){
        case IVEC  :
        case INT   : fort_out(fp,"        res=gft_write_id_int(");
                     break;
        case FLOAT : fort_out(fp,"        res=gft_write_id_float(");
                     break;
        case STRING: fort_out(fp,"        res=gft_write_id_str(");
                     break;
      }
      sprintf(forbuf,"'%s',",params[i].name->n);
      fort_out(fp,forbuf);
      sprintf(forbuf,"%s,%d)\n",params[i].name->n,vsize(params[i].size));
      fort_out(fp,forbuf);
    }
  }
  fort_out(fp,"        return\n");
  fort_out(fp,"      end\n\n");
}

void cleanup(FILE *fp)
{
  int fn,i,j;
  offset_type *o;
  work_list *wl;

  if(language==ALLF){
    fort_out(fp,"!----------------------------------------------------------------------\n");
    fort_out(fp,"!  cleanup\n");
    fort_out(fp,"!----------------------------------------------------------------------\n");
    fort_out(fp,"      subroutine cleanup\n");
    fort_out(fp,"        implicit none\n\n");
    fort_out(fp,"        include 'sys_param.inc'\n");
    fort_out(fp,"        include 'gfuni0.inc'\n");
    fort_out(fp,"        include 'globals.inc'\n\n");
    fort_out(fp,"        call gft_close_all()\n");
    fort_out(fp,"        return\n");
    fort_out(fp,"      end\n\n");
  }else{
     for(fn=0;fn<ngfuncs;fn++){
       for(o=gfuncs[fn].tlev;o!=NULL;o=o->next){
         if(!gfuncs[fn].alias || gfuncs[fn].ntlevs==1 || o->next!=NULL){
           fprintf(fp,"  free(%s",gfuncs[fn].fname->n);
           gfunc_suffix(fp,o->offset);
           fprintf(fp,");\n");
         }
       }
     }
     for(i=0,j=0;i<nresids;){
       if(resids[i].eval!=0){
         fprintf(fp,"  free(%s_res);\n",resids[i].gfunc->fname->n);
         j++;
       }
       for(++i;i<nresids && (resids[i].gfunc == resids[i-1].gfunc);i++);
     }
     for(wl=static_work;wl!=NULL;wl=wl->next){
        fprintf(fp," free(work%d);\n",wl->work->num);
     }
  }
}

void handler(FILE *fp)
{
  switch(language){
     case IDF :
     case UPF :
     case F77 :
     case F90 :
    case C   : handler_c(fp);
               break;
    case ALLF: handler_f77(fp);
               break;
  }
}

void handler_f77(FILE *fp)
{
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"!  handler\n");
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"      subroutine handler\n");
  fort_out(fp,"        implicit none\n\n");
  fort_out(fp,"        include 'sys_param.inc'\n");
  fort_out(fp,"        include 'gfuni0.inc'\n");
  fort_out(fp,"        include 'globals.inc'\n\n");
  fort_out(fp,"        character*(5) temp\n");
  fort_out(fp,"        integer quit,ch,i,encode\n");
  fort_out(fp,"        integer rs\n");
  fort_out(fp,"        quit=0\n");
  fort_out(fp,"99      format (a)\n");
  fort_out(fp,"98      format (i3,a)\n");
  fort_out(fp,"100     continue\n");
  fort_out(fp,"          write(*,*) '1.  Change output frequency'\n");
  fort_out(fp,"          write(*,*) '2.  View out_gf'\n");
  fort_out(fp,"          write(*,*) '3.  Change out_gf'\n");
  fort_out(fp,"          write(*,*) '4.  Resume'\n");
  fort_out(fp,"          write(*,*) '5.  Exit'\n");
  fort_out(fp,"          write(*,99) 'Enter choice: '\n");
  fort_out(fp,"          read(*,*,err=100) ch\n");
  fort_out(fp,"          if(ch .eq. 1) then\n");
  fort_out(fp,"            write(*,99) 'Enter new frequency 1/'\n");
  fort_out(fp,"            read(*,*) rmod\n");
  fort_out(fp,"          else if(ch .eq. 2) then\n");
  sprintf(forbuf,"            do i=1,%d\n",get_size_one());
  fort_out(fp,forbuf);
  fort_out(fp,"              write(*,98) i,'. '//attrfname(i)\n");
  fort_out(fp,"              if(out_gf(i) .eq. 1) then\n");
  fort_out(fp,"                write(*,*) ' ON'\n");
  fort_out(fp,"              else\n");
  fort_out(fp,"                write(*,*) ' OFF'\n");
  fort_out(fp,"              end if\n");
  fort_out(fp,"            end do\n");
  fort_out(fp,"          else if(ch .eq. 3) then\n");
  fort_out(fp,"            write(*,99) 'Enter grid function number: '\n");
  fort_out(fp,"            read(*,*) i\n");
  sprintf(forbuf,"            if(i .gt. 0 .and. i .le. %d) then\n",get_size_one());
  fort_out(fp,forbuf);
  fort_out(fp,"              out_gf(i)=mod(out_gf(i)+1,2)\n");
  fort_out(fp,"              if(out_gf(i) .eq. 0) then\n");
  fort_out(fp,"                call gft_close(attrfname(i))\n");
  fort_out(fp,"              end if\n");
  fort_out(fp,"            end if\n");
  fort_out(fp,"          else if(ch .eq. 4) then\n");
  fort_out(fp,"            quit=1\n");
  fort_out(fp,"          else if(ch .eq. 5) then\n");
  fort_out(fp,"            quit=1\n");
  fort_out(fp,"            rnpldone=1\n");
  fort_out(fp,"          end if\n");
  fort_out(fp,"        if(quit .ne. 1) then\n");
  fort_out(fp,"          goto 100\n");
  fort_out(fp,"        end if\n");
  sprintf(forbuf,"        nout_gf=encode(lout_gf,out_gf,%d)\n",get_size_one());
  fort_out(fp,forbuf);
  fort_out(fp,"        return\n");
  fort_out(fp,"      end\n\n");
}

void handler_c(FILE *fp)
{
  fprintf(fp,"void handler(int sig, int *rnpldone, int *rmod, char **fname, int *out_gf)\n");
  fprintf(fp,"{\n");
  fprintf(fp,"  char temp[5];\n");
  fprintf(fp,"  int quit,ch,i;\n\n");
  fprintf(fp,"  if(sig==SIGQUIT){\n");
  fprintf(fp,"    quit=1;\n    *rnpldone=1;\n");
  fprintf(fp,"  }else quit=0;\n");
  fprintf(fp,"  while(!quit){\n");
  fprintf(fp,"    fprintf(stdout,\"1.  Change output frequency\\n\");\n");
  fprintf(fp,"    fprintf(stdout,\"2.  View out_gf\\n\");\n");
  fprintf(fp,"    fprintf(stdout,\"3.  Change out_gf\\n\");\n");
  fprintf(fp,"    fprintf(stdout,\"4.  Resume\\n\");\n");
  fprintf(fp,"    fprintf(stdout,\"5.  Exit\\n\");\n");
  fprintf(fp,"    fprintf(stdout,\"Enter choice:\");\n");
  fprintf(fp,"    scanf(\"%%d\",&ch);\n");
  fprintf(fp,"    gets(temp);\n");
  fprintf(fp,"    switch(ch){\n");
  fprintf(fp,"      case 1 : fprintf(stdout,\"Enter new frequency: 1/\");\n");
  fprintf(fp,"               scanf(\"%%d\",rmod);\n");
  fprintf(fp,"               gets(temp);\n");
  fprintf(fp,"               break;\n");
  fprintf(fp,"      case 2 : for(i=0;i<%d;i++){\n",get_size_one());
  fprintf(fp,"                 fprintf(stdout,\"%%d. %%s\",i,fname[i]);\n");
  fprintf(fp,"                 if(out_gf[i])\n");
  fprintf(fp,"                   fprintf(stdout,\" ON\\n\");\n");
  fprintf(fp,"                 else fprintf(stdout,\" OFF\\n\");\n");
  fprintf(fp,"               }\n");
  fprintf(fp,"               break;\n");
  fprintf(fp,"      case 3 : fprintf(stdout,\"Enter grid function number: \");\n");
  fprintf(fp,"               scanf(\"%%d\",&i);\n");
  fprintf(fp,"               gets(temp);\n");
  fprintf(fp,"               if(i>=0 && i<%d){\n",get_size_one());
  fprintf(fp,"                 out_gf[i]=!out_gf[i];\n");
  fprintf(fp,"                 if(!out_gf[i]) gft_close(fname[i]);\n");
  fprintf(fp,"               }\n");
  fprintf(fp,"               break;\n");
  fprintf(fp,"      case 4 : quit=1;\n");
  fprintf(fp,"               break;\n");
  fprintf(fp,"      case 5 : quit=1; *rnpldone=1;\n");  
  fprintf(fp,"               break;\n");
  fprintf(fp,"    }\n");
  fprintf(fp,"  }\n");
  fprintf(fp,"}\n\n");
}

void dump_state(FILE *fp,int flag)
{
  switch(language){
     case IDF :
     case UPF :
     case F77 :
     case F90 :
    case C   : dump_state_c(fp,flag);
               break;
    case ALLF: dump_state_f77(fp);
               break;
  }
}

void dump_state_f77(FILE *fp)
{
  int fn,i,j;
  
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"!  dump_state\n");
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"      subroutine dump_state(t, st, f_name)\n");
  fort_out(fp,"        implicit none\n\n");
  fort_out(fp,"        include 'sys_param.inc'\n");
  fort_out(fp,"        include 'gfuni0.inc'\n");
  fort_out(fp,"        include 'globals.inc'\n");
  fort_out(fp,"        real*8  t\n");
  fort_out(fp,"        integer st, res\n");
  sprintf(forbuf,"        character*%d f_name\n",STR_P_SIZE);
  fort_out(fp,forbuf);
  fort_out(fp,"        integer gft_write_id_gf\n\n");
  fort_out(fp,"        call gft_set_single(f_name)\n");
  for(fn=0;fn<ngfuncs;fn++){
    if(gfuncs[fn].ntlevs==1){
      j=0;
      i=0;
    }else{
      j=gfuncs[fn].ntlevs-2;
      i=1;
    }
    while(j>=0){
      fort_out(fp,"        res=gft_write_id_gf(");
      sprintf(forbuf,"'%s[%d]',ptrs(getpshape(%d)),",gfuncs[fn].fname->n,j,
              gptr_to_index(gfuncs[fn].grd)+1);
      fort_out(fp,forbuf);
      sprintf(forbuf,"getrank(%d),q(ptrs(gf_st(%d)+%d)))\n",gptr_to_index(gfuncs[fn].grd)+1,fn+1,i+1);
      fort_out(fp,forbuf);
      i++;
      j--;
    }
  }
  fort_out(fp,"        start_t=t\n");
  fort_out(fp,"        s_step=st\n");
  fort_out(fp,"        call write_params_attribs()\n");
  fort_out(fp,"        call gft_set_multi()\n");
  fort_out(fp,"        return\n");
  fort_out(fp,"      end\n");
}

void dump_state_header(FILE *fp)
{
   int fn,i;
   offset_type *o;
   
  fprintf(fp,"void dump_state(const double t, lattice_type *lats");
  for(fn=0;fn<ngfuncs;fn++){
    if(gfuncs[fn].ntlevs==1){
      o=gfuncs[fn].tlev;
    }else{
      o=gfuncs[fn].tlev->next;
    }
    while(o!=NULL){
       switch(gfuncs[fn].type){
          case INT:
             fprintf(fp,",int *");
             break;
          case FLOAT:
             fprintf(fp,",double *");
             break;
       }
      fprintf(fp,"%s",gfuncs[fn].fname->n);
      gfunc_suffix(fp,o->offset);
      o=o->next;
    }
  }
   for(i=0;i<nparams;i++){
    if(params[i].con!=2){
      switch(params[i].type){
        case INT   : fprintf(fp,",int ");
                     break;
        case FLOAT : fprintf(fp,",double ");
                     break;
        case STRING: fprintf(fp,",char ");
                     break;
        case IVEC   : fprintf(fp,",int ");
                     break;
      }
      if(vsize(params[i].size)>1 || params[i].type==STRING)
        fprintf(fp,"*");
      fprintf(fp,"%s",params[i].name->n);
    }
   }
  fprintf(fp,")\n");
}
   
void dump_state_call(FILE *fp)
{
   int fn,i;
   offset_type *o;
   
  fprintf(fp,"dump_state(t, lats");
  for(fn=0;fn<ngfuncs;fn++){
    if(gfuncs[fn].ntlevs==1){
      o=gfuncs[fn].tlev;
    }else{
      o=gfuncs[fn].tlev->next;
    }
    while(o!=NULL){
      fprintf(fp,",%s",gfuncs[fn].fname->n);
      gfunc_suffix(fp,o->offset);
      o=o->next;
    }
  }
   for(i=0;i<nparams;i++){
    if(params[i].con!=2){
      fprintf(fp,",%s",params[i].name->n);
    }
   }
  fprintf(fp,");\n");
}

void dump_state_c(FILE *fp, int flag)
{
  int fn,i,j;
  offset_type *o;
  
  /* note: this function won't generate correct code for integer grid functions */
   dump_state_header(fp);
  fprintf(fp,"{\n");
  if(flag)
     fprintf(fp,"  gft_set_single(out_file);\n");
  else fprintf(fp,"  gft_set_single(in_file);\n");
  for(fn=0;fn<ngfuncs;fn++){
    if(gfuncs[fn].ntlevs==1){
      o=gfuncs[fn].tlev;
      i=0;
    }else{
      o=gfuncs[fn].tlev->next;
      i=gfuncs[fn].ntlevs-2;
    }
    while(o!=NULL){
      if(gfuncs[fn].type==INT){
      }
      fprintf(fp,"  gft_write_id_gf(\"%s\",lats[%d].shape,lats[%d].rank,%s",
              gfuncs[fn].fname->n,gptr_to_index(gfuncs[fn].grd),gptr_to_index(gfuncs[fn].grd),
              gfuncs[fn].fname->n);
      gfunc_suffix(fp,o->offset);
      fprintf(fp,");\n");
      i--;
      o=o->next;
    }
  }
  fprintf(fp,"  start_t=t;\n");
  for(i=0;i<nparams;i++){
    if(params[i].con!=2){
      switch(params[i].type){
        case IVEC   :
        case INT   : fprintf(fp,"  gft_write_id_int(");
                     break;
        case FLOAT : fprintf(fp,"  gft_write_id_float(");
                     break;
        case STRING: fprintf(fp,"  gft_write_id_str(");
                     break;
      }
      fprintf(fp,"\"%s\",",params[i].name->n);
      if(vsize(params[i].size)==1)
        fprintf(fp,"&");
      fprintf(fp,"%s,%d);\n",params[i].name->n,vsize(params[i].size));
    }
  }
  fprintf(fp,"  gft_set_multi();\n");
  fprintf(fp,"}\n\n");
}

void output_header(FILE *fp)
{
  int fn,rn,i;
  offset_type *o;
  
  if(language==ALLF){
    fort_out(fp,"!----------------------------------------------------------------------\n");
    fort_out(fp,"!  output_gfuncs\n");
    fort_out(fp,"!----------------------------------------------------------------------\n");
    fort_out(fp,"      subroutine output_gfuncs(o_fout, o_ser, t, o_out_gf, fname,");
    for(fn=0;fn<ngfuncs;fn++){
      sprintf(forbuf,"%s",gfuncs[fn].fname->n);
      fort_out(fp,forbuf);
      if(gfuncs[fn].ntlevs==1){
        o=gfuncs[fn].tlev;
      }else{
        o=gfuncs[fn].tlev->next;
      }
      gfunc_suffix_f(fp,o->offset);
      fort_out(fp,",");
      if((rn=resid_exists(gfuncs[fn].fname))!=-1){
        if(resids[rn].eval){
          sprintf(forbuf,"%s_res,",gfuncs[fn].fname->n);
          fort_out(fp,forbuf);
        }
      }
    }
    for(i=0;i<ngrids;i++){
      sprintf(forbuf,"%s_rank,%s_shape,%s_cnames,%s_crds",
              grids[i].name->n,grids[i].name->n,grids[i].name->n,grids[i].name->n);
      fort_out(fp,forbuf);
      if(i<ngrids-1)
        fort_out(fp,",");
    }
    fort_out(fp,")\n");
    fort_out(fp,"        implicit none\n\n");
    fort_out(fp,"        include 'sys_param.inc'\n");
    fort_out(fp,"        include 'gfuni0.inc'\n");
    fort_out(fp,"        include 'globals.inc'\n");
    fort_out(fp,"        integer o_fout, o_ser\n");
    fort_out(fp,"        real*8  t\n");
    sprintf(forbuf,"        integer o_out_gf(%d)\n",get_size_one());
    fort_out(fp,forbuf);
    sprintf(forbuf,"        character*%d fname(%d)\n",STR_P_SIZE,get_size_one());
    fort_out(fp,forbuf);
    for(fn=0;fn<ngfuncs;fn++){
      switch(gfuncs[fn].type){
        case INT   : fort_out(fp,"        integer");
                     break;
        case FLOAT : fort_out(fp,"        real*8");
                     break;
      }
      sprintf(forbuf," %s",gfuncs[fn].fname->n);
      fort_out(fp,forbuf);
      if(gfuncs[fn].ntlevs==1){
        o=gfuncs[fn].tlev;
      }else{
        o=gfuncs[fn].tlev->next;
      }
      gfunc_suffix_f(fp,o->offset);
      fort_out(fp,"\n");
      if((rn=resid_exists(gfuncs[fn].fname))!=-1){
        if(resids[rn].eval){
          switch(gfuncs[fn].type){
            case INT   : fort_out(fp,"        integer");
                         break;
            case FLOAT : fort_out(fp,"        real*8");
                         break;
          }
          sprintf(forbuf," %s_res\n",gfuncs[fn].fname->n);
          fort_out(fp,forbuf);
        }
      }
    }
    for(i=0;i<ngrids;i++){
      sprintf(forbuf,"        integer %s_rank\n",grids[i].name->n);
      fort_out(fp,forbuf);
      sprintf(forbuf,"        integer %s_shape(*)\n",grids[i].name->n);
      fort_out(fp,forbuf);
      sprintf(forbuf,"        character*%d %s_cnames\n",STR_P_SIZE,grids[i].name->n);
      fort_out(fp,forbuf);
      sprintf(forbuf,"        real*8 %s_crds\n",grids[i].name->n);
      fort_out(fp,forbuf);
    }
    fort_out(fp,"        integer res\n");
    fort_out(fp,"        integer gft_out_full,rvsxynt\n\n");
  }else{
    fprintf(fp,"void output_gfuncs(int fout, int ser, double t, int *out_gf, char **fname,");
    for(fn=0;fn<ngfuncs;fn++){
      switch(gfuncs[fn].type){
        case INT   : fprintf(fp,"int *");
                     break;
        case FLOAT : fprintf(fp,"double *");
                     break;
      }
      fprintf(fp,"%s",gfuncs[fn].fname->n);
      if(gfuncs[fn].ntlevs==1){
        o=gfuncs[fn].tlev;
      }else{
        o=gfuncs[fn].tlev->next;
      }
      gfunc_suffix(fp,o->offset);
      fprintf(fp,",");
      if((rn=resid_exists(gfuncs[fn].fname))!=-1){
        if(resids[rn].eval){
          switch(gfuncs[fn].type){
            case INT   : fprintf(fp,"int *");
                         break;
            case FLOAT : fprintf(fp,"double *");
                         break;
          }
          fprintf(fp," %s_res,",gfuncs[fn].fname->n);
        }
      }
    }
    for(i=0;i<ngrids;i++){
      fprintf(fp,"int %s_rank, int *%s_shape, char *%s_cnames, double *%s_crds",
              grids[i].name->n,grids[i].name->n,grids[i].name->n,grids[i].name->n);
      if(i<ngrids-1)
        fprintf(fp,",");
    }
    fprintf(fp,")\n{\n");
  }
}

void output_call(FILE *fp)
{
  int fn,rn,i,j;
  offset_type *o;

  if(language==ALLF){
    j=0;
    fort_out(fp,"call output_gfuncs(fout,ser,t,out_gf,attrfname,");
    for(fn=0;fn<ngfuncs;fn++){
      if(gfuncs[fn].ntlevs==1){
        i=0;
      }else{
        i=1;
      }
      sprintf(forbuf,"q(ptrs(gf_st(%d)+%d)),",fn+1,i+1);
      fort_out(fp,forbuf);
      if((rn=resid_exists(gfuncs[fn].fname))!=-1){
        if(resids[rn].eval){
          sprintf(forbuf,"q(ptrs(gf_st(%d)+1)),",ngfuncs+j+1);
          fort_out(fp,forbuf);
          j++;
        }
      }
    }
    for(i=0;i<ngrids;i++){
      sprintf(forbuf,"getrank(%d),",i+1);
      fort_out(fp,forbuf);
      sprintf(forbuf,"ptrs(getpshape(%d)),",i+1);
      fort_out(fp,forbuf);
      sprintf(forbuf,"cname(getlatcs(%d)),",i+1);
      fort_out(fp,forbuf);
      sprintf(forbuf,"q(getpcoord(%d,1))",i+1);
      fort_out(fp,forbuf);
      if(i<ngrids-1)
        fort_out(fp,",");
    }
    fort_out(fp,")\n");
  }else{
    fprintf(fp,"output_gfuncs(fout,ser,t,out_gf,fname,");
    for(fn=0;fn<ngfuncs;fn++){
      fprintf(fp,"%s",gfuncs[fn].fname->n);
      if(gfuncs[fn].ntlevs==1){
        o=gfuncs[fn].tlev;
      }else{
        o=gfuncs[fn].tlev->next;
      }
      gfunc_suffix(fp,o->offset);
      fprintf(fp,",");
      if((rn=resid_exists(gfuncs[fn].fname))!=-1){
        if(resids[rn].eval)
          fprintf(fp,"%s_res,",gfuncs[fn].fname->n);
      }
    }
    for(i=0;i<ngrids;i++){
      fprintf(fp,"lats[%d].rank,lats[%d].shape,cname[lats[%d].cs],lats[%d].coords",
              i,i,i,i);
      if(i<ngrids-1)
        fprintf(fp,",");
    }
    fprintf(fp,");\n");
  }
}

/* generate code for output control */
void output_func(FILE *fp)
{
  switch(language){
     case IDF :
     case UPF :
     case F77 :
     case F90 :
    case C   : output_func_c(fp);
               break;
    case ALLF: output_func_f77(fp);
               break;
  }
}

void output_func_f77(FILE *fp)
{
  int fn,j,rn,n;
  offset_type *o;
  
  output_header(fp);
  /* note: this function won't generate correct code for integer grid functions */
  for(fn=0;fn<ngfuncs;fn++){
    if(gfuncs[fn].ntlevs==1){
      o=gfuncs[fn].tlev;
    }else{
      o=gfuncs[fn].tlev->next;
    }
    if(gfuncs[fn].type==INT){
    }
    fprintf(fp,"        if(o_out_gf(%d).eq.1) then\n",fn+1);
    fprintf(fp,"          if(o_fout.eq.1) then\n");
    sprintf(forbuf,"            res=gft_out_full(fname(%d),t,",fn+1);
    fort_out(fp,forbuf);
    sprintf(forbuf,"%s_shape,",gfuncs[fn].grd->name->n);
    fort_out(fp,forbuf);
    sprintf(forbuf,"%s_cnames,",gfuncs[fn].grd->name->n);
    fort_out(fp,forbuf);
    sprintf(forbuf,"%s_rank,",gfuncs[fn].grd->name->n);
    fort_out(fp,forbuf);
    sprintf(forbuf,"%s_crds,",gfuncs[fn].grd->name->n);
    fort_out(fp,forbuf);
    sprintf(forbuf,"%s",gfuncs[fn].fname->n);
    fort_out(fp,forbuf);
    gfunc_suffix_f(fp,o->offset);
    fort_out(fp,")\n");
    fprintf(fp,"          end if\n");
    if(gfuncs[fn].grd->reg.rank==1){
      fprintf(fp,"          if(o_ser.eq.1) then\n");
      sprintf(forbuf,"            res=rvsxynt(fname(%d),t,",fn+1);
      fort_out(fp,forbuf);
      sprintf(forbuf,"%s_crds,",gfuncs[fn].grd->name->n);
      fort_out(fp,forbuf);
      sprintf(forbuf,"%s",gfuncs[fn].fname->n);
      fort_out(fp,forbuf);
      gfunc_suffix_f(fp,o->offset);
      sprintf(forbuf,",%s_shape(1))\n",gfuncs[fn].grd->name->n);
      fort_out(fp,forbuf);
      fprintf(fp,"          end if\n");
    }
    fprintf(fp,"        end if\n");
  }
  for(rn=0,n=0;rn<nresids;){
    if(resids[rn].eval){
      fprintf(fp,"        if(o_out_gf(%d).eq.1) then\n",ngfuncs+n+1);
      fprintf(fp,"          if(o_fout.eq.1) then\n");
      sprintf(forbuf,"            res=gft_out_full(fname(%d),t,",ngfuncs+n+1);
      fort_out(fp,forbuf);
      sprintf(forbuf,"%s_shape,",resids[rn].gfunc->grd->name->n);
      fort_out(fp,forbuf);
      sprintf(forbuf,"%s_cnames,",resids[rn].gfunc->grd->name->n);
      fort_out(fp,forbuf);
      sprintf(forbuf,"%s_rank,",resids[rn].gfunc->grd->name->n);
      fort_out(fp,forbuf);
      sprintf(forbuf,"%s_crds,",resids[rn].gfunc->grd->name->n);
      fort_out(fp,forbuf);
      sprintf(forbuf,"%s_res",resids[rn].gfunc->fname->n);
      fort_out(fp,forbuf);
      fort_out(fp,")\n");
      fprintf(fp,"          end if\n");
      if(resids[rn].gfunc->grd->reg.rank==1){
        fprintf(fp,"          if(o_ser.eq.1) then\n");
        sprintf(forbuf,"            res=rvsxynt(fname(%d),t,",ngfuncs+n+1);
        fort_out(fp,forbuf);
        sprintf(forbuf,"%s_crds,",resids[rn].gfunc->grd->name->n);
        fort_out(fp,forbuf);
        sprintf(forbuf,"%s_res",resids[rn].gfunc->fname->n);
        fort_out(fp,forbuf);
        sprintf(forbuf,",%s_shape(1))\n",resids[rn].gfunc->grd->name->n);
        fort_out(fp,forbuf);
        fprintf(fp,"          end if\n");
      }
      fprintf(fp,"        end if\n");
      n++;
    }
    for(++rn;rn<nresids && (resids[rn].gfunc == resids[rn-1].gfunc);rn++);
  }
  fort_out(fp,"        return\n");
  fort_out(fp,"      end\n");
}

void output_func_c(FILE *fp)
{
  int fn,rn,n;
  offset_type *o;
  
  /* note: this function won't generate correct code for integer grid functions */
  output_header(fp);
  for(fn=0;fn<ngfuncs;fn++){
    if(gfuncs[fn].ntlevs==1){
      o=gfuncs[fn].tlev;
    }else{
      o=gfuncs[fn].tlev->next;
    }
    if(gfuncs[fn].type==INT){
    }
    fprintf(fp,"  if(out_gf[%d]){\n",fn);
    fprintf(fp,"    if(fout)\n");
    fprintf(fp,"      gft_out_full(fname[%d],t,%s_shape,%s_cnames,",
            fn,gfuncs[fn].grd->name->n,gfuncs[fn].grd->name->n);
    fprintf(fp,"%s_rank,%s_crds,%s",gfuncs[fn].grd->name->n,
            gfuncs[fn].grd->name->n,gfuncs[fn].fname->n);
    gfunc_suffix(fp,o->offset);
    fprintf(fp,");\n");
    if(gfuncs[fn].grd->reg.rank==1){
      fprintf(fp,"    if(ser)\n");
      fprintf(fp,"      rvsxynt(fname[%d],t,%s_crds,%s",fn,
              gfuncs[fn].grd->name->n,gfuncs[fn].fname->n);
      gfunc_suffix(fp,o->offset);
      fprintf(fp,",%s_shape[0]);\n",gfuncs[fn].grd->name->n);
    }
    fprintf(fp,"  }\n");
  }
  for(rn=0,n=0;rn<nresids;){
    if(resids[rn].eval){
      fprintf(fp,"  if(out_gf[%d]){\n",ngfuncs+n);
      fprintf(fp,"    if(fout)\n");
      fprintf(fp,"      gft_out_full(fname[%d],t,%s_shape,%s_cnames,",
              ngfuncs+n,resids[rn].gfunc->grd->name->n,
              resids[rn].gfunc->grd->name->n);
      fprintf(fp,"%s_rank,%s_crds,%s_res",resids[rn].gfunc->grd->name->n,
              resids[rn].gfunc->grd->name->n,resids[rn].gfunc->fname->n);
      fprintf(fp,");\n");
      if(resids[rn].gfunc->grd->reg.rank==1){
        fprintf(fp,"    if(ser)\n");
        fprintf(fp,"      rvsxynt(fname[%d],t,%s_crds,%s_res",ngfuncs+n,
                resids[rn].gfunc->grd->name->n,resids[rn].gfunc->fname->n);
        fprintf(fp,",%s_shape[0]);\n",resids[rn].gfunc->grd->name->n);
      }
      n++;
      fprintf(fp,"  }\n");
    }
    for(++rn;rn<nresids && (resids[rn].gfunc == resids[rn-1].gfunc);rn++);
  }
  fprintf(fp,"}\n\n");
}

/* generate code to initialize each grid function */

void init_header(FILE *fp, const update_table *up)
{
  gfunc_ref_list *gl;
  coord_list *cl;
  i_reg *ir;
  int i,j,fl;
  gfunc_tab_list *gt;
  work_list *wl;
  param_ref_list *pr;
  
  fl=!mystrcmp("auto",up->type->n);
  if(language==C || language==UPF){
    fprintf(fp,"/*  This routine initializes the following grid functions \n");
    fprintf(fp,"    ");
    for(gt=up->gfs;gt!=NULL;gt=gt->next)
      fprintf(fp,"%s ",gt->gfunc->fname->n);
    fprintf(fp,"\n*/\n");
    if(up->name)
      fprintf(fp,"void %s(",up->name->n);
    else
      fprintf(fp,"void initializer%d(",initer_to_index(up));
    for(gl=up->glob_gf;gl!=NULL;gl=gl->next){
      switch(gl->gfunc->type){
        case INT   : fprintf(fp,"int *");
                     break;
        case FLOAT : fprintf(fp,"double *");
                     break;
      }
      if(!gl->name){
        fprintf(fp,"%s",gl->gfunc->fname->n);
        gfunc_suffix(fp,gl->toff);
      }else fprintf(fp,"%s",gl->name->n);
      if((gl->next && gl->next->gfunc->grd != gl->gfunc->grd) || !gl->next){
        for(i=0,cl=gl->gfunc->grd->clst;i<gl->gfunc->grd->reg.rank;i++,cl=cl->next){
          fprintf(fp,",int %s_N%s",gl->gfunc->grd->name->n,cl->name->n);
        }
      }
      if(gl->next) fprintf(fp,",");
    }
    for(cl=up->glob_crds;cl!=NULL;cl=cl->next){
      if(is_space(cl->name))
        fprintf(fp,",double *%s",cl->name->n);
      else fprintf(fp,",double %s",cl->name->n);
    }
    for(cl=up->glob_cdifs;cl!=NULL;cl=cl->next){
      fprintf(fp,",double %s",cl->name->n);
    }
    for(pr=up->glob_par;pr!=NULL;pr=pr->next){
      switch(pr->par->type){
        case  INT    :  fprintf(fp,",int ");
                      break;
        case  FLOAT  :  fprintf(fp,",double ");
                      break;
      }
      if(vsize(pr->par->size)>1)
        fprintf(fp,"*");
      fprintf(fp,"%s",pr->par->name->n);
    }
    for(wl=up->work_refs;wl!=NULL;wl=wl->next)
      fprintf(fp,",double *work%d, int nwork%d",wl->work->num,wl->work->num);
    fprintf(fp,")\n");
    fprintf(fp,"{\n");
    if(fl)
      fprintf(fp,"  int i,j,k;\n");
    fprintf(fp,"\n");
  }else if(language==F77 || language==F90 || language==ALLF || language==IDF){
    int ar=0;
    fort_out(fp,"\n");
    fort_out(fp,"!----------------------------------------------------------------------\n");
    fort_out(fp,"!  This routine initializes the following grid functions\n");
    fort_out(fp,"!  ");
    for(gt=up->gfs;gt!=NULL;gt=gt->next){
      sprintf(forbuf,"%s ",gt->gfunc->fname->n);
      fortcom_out(fp,forbuf);
    }
    fort_out(fp,"\n");
    fort_out(fp,"!----------------------------------------------------------------------\n");
    if(up->name)
      sprintf(forbuf,"      subroutine %s(",up->name->n);
    else
      sprintf(forbuf,"      subroutine initializer%d(",initer_to_index(up));
    fort_out(fp,forbuf);
    for(gl=up->glob_gf;gl!=NULL;gl=gl->next){
      if(!gl->name){
        fort_out(fp,gl->gfunc->fname->n);
        gfunc_suffix_f(fp,gl->toff);
      }else{
        fort_out(fp,gl->name->n);
        if(gl->array)
          while(gl->next && gl->gfunc==gl->next->gfunc)
            gl=gl->next;
      }
      if((gl->next && gl->next->gfunc->grd != gl->gfunc->grd) || !gl->next){
        for(i=0,cl=gl->gfunc->grd->clst;i<gl->gfunc->grd->reg.rank;i++,cl=cl->next){
          fort_out(fp,",");
          sprintf(forbuf,"%s_N%s",gl->gfunc->grd->name->n,cl->name->n);
          fort_out(fp,forbuf);
        }
      }
      if(gl->next) fort_out(fp,",");
    }
    if(up->glob_crds){
      fort_out(fp,",");
      for(cl=up->glob_crds;cl!=NULL;cl=cl->next){
        fort_out(fp,cl->name->n);
        if(cl->next)
          fort_out(fp,",");
      }
    }
    if(up->glob_cdifs){
      fort_out(fp,",");
      for(cl=up->glob_cdifs;cl!=NULL;cl=cl->next){
        fort_out(fp,cl->name->n);
        if(cl->next)
          fort_out(fp,",");
      }
    }
    if(up->glob_par){
      fort_out(fp,",");
      for(pr=up->glob_par;pr!=NULL;pr=pr->next){
        fort_out(fp,pr->par->name->n);
        if(pr->next)
          fort_out(fp,",");
      }
    }
    if(up->work_refs){
      fort_out(fp,",");    
      for(wl=up->work_refs;wl!=NULL;wl=wl->next){
        sprintf(forbuf,"work%d,",wl->work->num);
        fort_out(fp,forbuf);
        sprintf(forbuf,"nwork%d",wl->work->num);
        fort_out(fp,forbuf);
        if(wl->next!=NULL)
          fort_out(fp,",");
      }
    }  
    fort_out(fp,")\n");
    fort_out(fp,"        implicit none\n\n");
      if(language==ALLF)
       fort_out(fp,"        include 'globals.inc'\n\n");
    if(gl=up->glob_gf){
      fort_out(fp,"        integer ");
      for(i=0,cl=gl->gfunc->grd->clst;i<gl->gfunc->grd->reg.rank;i++,cl=cl->next){
        sprintf(forbuf,"%s_N%s",gl->gfunc->grd->name->n,cl->name->n);
        fort_out(fp,forbuf);
        if(i<gl->gfunc->grd->reg.rank-1)
          fort_out(fp,",");
        else fort_out(fp,"\n");
      }
    }
    for(;gl!=NULL;gl=gl->next){
      switch(gl->gfunc->type){
        case INT    : fort_out(fp,"        integer ");
                      break;
        case FLOAT  : fort_out(fp,"        real*8  ");
                      break;
        case COMPLEX:  fort_out(fp,"        complex ");
                      break;
      }
      if(!gl->name){
        fort_out(fp,gl->gfunc->fname->n);
        gfunc_suffix_f(fp,gl->toff);
      }else{
        fort_out(fp,gl->name->n);
        if(gl->array){
          while(gl->next && gl->gfunc==gl->next->gfunc)
            gl=gl->next;
          ar=1;
        }
      }
      array_bounds_to_string(fp,gl->gfunc->grd,ar);
      if(ar){
        sprintf(forbuf,",%d)",gl->gfunc->ntlevs);
        fort_out(fp,forbuf);
      }
      ar=0;
      fort_out(fp,"\n");
      if((gl->next && gl->next->gfunc->grd != gl->gfunc->grd)){
        fort_out(fp,"        integer ");
        for(i=0,cl=gl->next->gfunc->grd->clst;i<gl->next->gfunc->grd->reg.rank;i++,cl=cl->next){
          sprintf(forbuf,"%s_N%s",gl->next->gfunc->grd->name->n,cl->name->n);
          fort_out(fp,forbuf);
          if(i<gl->next->gfunc->grd->reg.rank-1)
            fort_out(fp,",");
          else fort_out(fp,"\n");
        }
      }
    }
    for(cl=up->glob_crds;cl!=NULL;cl=cl->next){
      if(is_space(cl->name)){
        sprintf(forbuf,"        real*8  %s(*)\n",cl->name->n);
        fort_out(fp,forbuf);
      }else{
        sprintf(forbuf,"        real*8  %s\n",cl->name->n);
        fort_out(fp,forbuf);
      }
    }
    for(cl=up->glob_cdifs;cl!=NULL;cl=cl->next){
      sprintf(forbuf,"        real*8  %s\n",cl->name->n);
      fort_out(fp,forbuf);
    }
    for(pr=up->glob_par;pr!=NULL;pr=pr->next){
      switch(pr->par->type){
        case  INT    :  fort_out(fp,"        integer ");
                      break;
        case  FLOAT  :  fort_out(fp,"        real*8  ");
                      break;
      }
      fort_out(fp,pr->par->name->n);
      if(vsize(pr->par->size)>1){
        fort_out(fp,"(");
        for(j=0;j<pr->par->size->dim;j++){
          sprintf(forbuf,"%d",pr->par->size->size[j]);
          fort_out(fp,forbuf);
          if(j<pr->par->size->dim-1)
            fort_out(fp,",");
        }
        fort_out(fp,")");
      }
      fort_out(fp,"\n");
    }
    for(wl=up->work_refs;wl!=NULL;wl=wl->next){
      sprintf(forbuf,"        integer nwork%d\n",wl->work->num);
      fort_out(fp,forbuf);
      sprintf(forbuf,"        real*8  work%d(nwork%d)\n",wl->work->num,
              wl->work->num);
      fort_out(fp,forbuf);
    }
    if(fl){
      fort_out(fp,"        integer i,j,k\n");
      /*
      for(j=0;j<ncoords;j++)
         for(i=0;i<coords[j].rank-1;i++){
            sprintf(forbuf,"        integer %s\n",grid_base[j][i]->n);
            fort_out(fp,forbuf);
         }    
      fort_out(fp,"\n");
      for(j=0;j<ncoords;j++)
        for(i=0;i<coords[j].rank-1;i++){
          sprintf(forbuf,"        %s = %s0 * 2**level + 1\n",grid_base[j][i]->n,
                  grid_base[j][i]->n);
          fort_out(fp,forbuf);
        }
      */
    }
    fort_out(fp,"\n");
  }
}

/* output function call to initializer up.  Caller must indent */
void init_call(FILE *fp, const update_table *up)
{
  gfunc_ref_list *gl;
  coord_list *cl;
  i_reg *ir;
  int i,j;
  work_list *wl;
  param_ref_list *pr;
  
  if(language==C || language==UPF){
    if(up->name)
      fprintf(fp,"%s(",up->name->n);
    else
      fprintf(fp,"initializer%d(",initer_to_index(up));
    for(gl=up->glob_gf;gl!=NULL;gl=gl->next){
      fprintf(fp,"%s",gl->gfunc->fname->n);
      gfunc_suffix(fp,gl->toff);
      if((gl->next && gl->next->gfunc->grd != gl->gfunc->grd) || !gl->next){
        for(i=0;i<gl->gfunc->grd->reg.rank;i++)
          fprintf(fp,",lats[%d].shape[%d]",gptr_to_index(gl->gfunc->grd),i);
      }
      if(gl->next) fprintf(fp,",");
    }
    for(cl=up->glob_crds;cl!=NULL;cl=cl->next){
      fprintf(fp,",%s",cl->name->n);
    }
    for(cl=up->glob_cdifs;cl!=NULL;cl=cl->next){
      fprintf(fp,",%s",cl->name->n);
    }
    for(pr=up->glob_par;pr!=NULL;pr=pr->next){
      fprintf(fp,",%s",pr->par->name->n);
    }
    for(wl=up->work_refs;wl!=NULL;wl=wl->next)
      fprintf(fp,",work%d,nwork%d",wl->work->num,wl->work->num);
    fprintf(fp,");\n");
  }else if(language==F77 || language==F90 || language==IDF){
     char nm[256];
    if(up->name){
       strcpy(nm,up->name->n);
    }else{
      sprintf(nm,"initializer%d",initer_to_index(up));
    }
    fort_call(nm);
    fprintf(fp,"%s(",nm);
    for(gl=up->glob_gf;gl!=NULL;gl=gl->next){
      fprintf(fp,"%s",gl->gfunc->fname->n);
      gfunc_suffix(fp,gl->toff);
      if((gl->next && gl->next->gfunc->grd != gl->gfunc->grd) || !gl->next){
        for(i=0;i<gl->gfunc->grd->reg.rank;i++)
          fprintf(fp,",&lats[%d].shape[%d]",gptr_to_index(gl->gfunc->grd),i);
      }
      if(gl->next) fprintf(fp,",");
    }
    for(cl=up->glob_crds;cl!=NULL;cl=cl->next){
      if(is_space(cl->name))
        fprintf(fp,",%s",cl->name->n);
      else fprintf(fp,",&%s",cl->name->n);
    }
    for(cl=up->glob_cdifs;cl!=NULL;cl=cl->next){
      fprintf(fp,",&%s",cl->name->n);
    }
    for(pr=up->glob_par;pr!=NULL;pr=pr->next){
       if(vsize(pr->par->size)==1)
         fprintf(fp,",&%s",pr->par->name->n);
      else fprintf(fp,",%s",pr->par->name->n);
    }
    for(wl=up->work_refs;wl!=NULL;wl=wl->next)
      fprintf(fp,",work%d,&nwork%d",wl->work->num,wl->work->num);
    fprintf(fp,");\n");
  }else if(language==ALLF){
    if(up->name)
      sprintf(forbuf,"call %s(",up->name->n);
    else
      sprintf(forbuf,"call initializer%d(",initer_to_index(up));
    fort_out(fp,forbuf);
    for(gl=up->glob_gf;gl!=NULL;gl=gl->next){
      sprintf(forbuf,"q(ptrs(gf_st(%d)+%d))",gfunc_to_index(gl->gfunc)+1,
              offset_to_index(gl->toff,gl->gfunc->tlev)+1);
      fort_out(fp,forbuf);
      if(gl->array)
        while(gl->next && gl->gfunc==gl->next->gfunc)
          gl=gl->next;
      if((gl->next && gl->next->gfunc->grd != gl->gfunc->grd) || !gl->next){
        for(i=0;i<gl->gfunc->grd->reg.rank;i++){
          fort_out(fp,",");
          sprintf(forbuf,"ptrs(getpshape(%d)+%d)",gptr_to_index(gl->gfunc->grd)+1,i);
          fort_out(fp,forbuf);
        }
      }
      if(gl->next) fort_out(fp,",");
    }
    if(up->glob_crds){
      fort_out(fp,",");
      for(cl=up->glob_crds;cl!=NULL;cl=cl->next){
        if(is_space(cl->name))
          sprintf(forbuf,"q(%s)",cl->name->n);
        else sprintf(forbuf,"%s",cl->name->n);
        fort_out(fp,forbuf);
        if(cl->next)
          fort_out(fp,",");
      }
    }
    if(up->glob_cdifs){
      fort_out(fp,",");
      for(cl=up->glob_cdifs;cl!=NULL;cl=cl->next){
        fort_out(fp,cl->name->n);
        if(cl->next)
          fort_out(fp,",");
      }
    }
    if(up->glob_par){
      fort_out(fp,",");
      for(pr=up->glob_par;pr!=NULL;pr=pr->next){
        fort_out(fp,pr->par->name->n);
        if(pr->next)
          fort_out(fp,",");
      }
    }
    if(up->work_refs){
      fort_out(fp,",");    
      for(wl=up->work_refs;wl!=NULL;wl=wl->next){
        sprintf(forbuf,"q(work%d),",wl->work->num);
        fort_out(fp,forbuf);
        sprintf(forbuf,"nwork%d",wl->work->num);
        fort_out(fp,forbuf);
        if(wl->next)
          fort_out(fp,",");
      }
    }  
    fort_out(fp,")\n");
  }
}

void output_init_stat(FILE *fp, init_table *in)
{
  int j;
  char c;
  i_reg *ir;
  offset_type *o;

  if(language==C || language==UPF){
    if(in->etype==1){
      fprintf(fp,"    *(%s",in->gfunc->fname->n);
      /*
      for(j=0,o=in->gfunc->tlev;j<in->gfunc->ntlevs-1;o=o->next,j++);
      gfunc_suffix(fp,o->offset);
      */
      gfunc_suffix(fp,in->toff);
      fprintf(fp," + ");
      array_ref0_to_string(fp,in->gfunc->grd);
      fprintf(fp,")="); 
      ex_to_string(fp,in->elst.expr);
      fprintf(fp,";\n");
    }else{
      int done=0;
      ifstat *is;
      is=in->elst.ifst;
      fprintf(fp,"   ");
      do{
        fprintf(fp," if(");
        ex_to_string(fp,is->lexpr);
        fprintf(fp,")\n");
        fprintf(fp,"      *(%s",in->gfunc->fname->n);
        /*
        for(j=0,o=in->gfunc->tlev;j<in->gfunc->ntlevs-1;o=o->next,j++);
        gfunc_suffix(fp,o->offset);
        */
        gfunc_suffix(fp,in->toff);
        fprintf(fp," + ");
        array_ref0_to_string(fp,in->gfunc->grd);
        fprintf(fp,")="); 
        ex_to_string(fp,is->expr);
        fprintf(fp,";\n");
        if(is->etype==0){
          done=1;
        }else if(is->etype==1){
          fprintf(fp,"    else\n");
          fprintf(fp,"      *(%s",in->gfunc->fname->n);
          /*
          for(j=0,o=in->gfunc->tlev;j<in->gfunc->ntlevs-1;o=o->next,j++);
          gfunc_suffix(fp,o->offset);
          */
          gfunc_suffix(fp,in->toff);
          fprintf(fp," + ");
          array_ref0_to_string(fp,in->gfunc->grd);
          fprintf(fp,")="); 
          ex_to_string(fp,is->elst.expr);
          fprintf(fp,";\n");
          done=1;
        }else if(is->etype==2){
          fprintf(fp,"    else");
          is=is->elst.ifst;
        }
      }while(!done);
    }
  }else if(language==F77 || language==F90 || language==ALLF || language==IDF){
    if(in->etype==1){
      sprintf(forbuf,"          %s",in->gfunc->fname->n);
      fort_out(fp,forbuf);
      /*
      for(j=0,o=in->gfunc->tlev;j<in->gfunc->ntlevs-1;o=o->next,j++);
      gfunc_suffix_f(fp,o->offset);
      */
      gfunc_suffix_f(fp,in->toff);
      array_ref0_to_string_f(fp,in->gfunc->grd);
      fort_out(fp,"=");
      ex_to_string_f(fp,in->elst.expr);
      fort_out(fp,"\n");
    }else{
      int done=0;
      ifstat *is;
      is=in->elst.ifst;
      fort_out(fp,"        ");
      do{
        fort_out(fp," if(");
        ex_to_string_f(fp,is->lexpr);
        fort_out(fp,") then\n");
        sprintf(forbuf,"            %s",in->gfunc->fname->n);
        fort_out(fp,forbuf);
        /*
        for(j=0,o=in->gfunc->tlev;j<in->gfunc->ntlevs-1;o=o->next,j++);
        gfunc_suffix_f(fp,o->offset);
        */
        gfunc_suffix_f(fp,in->toff);
        array_ref0_to_string_f(fp,in->gfunc->grd);
        fort_out(fp,"=");
        ex_to_string_f(fp,is->expr);
        fort_out(fp,"\n");
        if(is->etype==0){
          done=1;
        }else if(is->etype==1){
          fort_out(fp,"           else\n");
          sprintf(forbuf,"            %s",in->gfunc->fname->n);
          fort_out(fp,forbuf);
          /*
           for(j=0,o=in->gfunc->tlev;j<in->gfunc->ntlevs-1;o=o->next,j++);
           gfunc_suffix_f(fp,o->offset);
           */
           gfunc_suffix_f(fp,in->toff);
          array_ref0_to_string_f(fp,in->gfunc->grd);
          fort_out(fp,"=");
          ex_to_string_f(fp,is->elst.expr);
          fort_out(fp,"\n");
          done=1;
        }else if(is->etype==2){
          fort_out(fp,"           else");
          is=is->elst.ifst;
        }
      }while(!done);
      fort_out(fp,"           end if\n");
    }
  }
}

void gen_gfuncs(FILE *fp)
{
  switch(language){
     case UPF :
    case C   : gen_gfuncs_c(fp);
               break;
    case IDF :
    case F90 :
    case ALLF:
    case F77 : gen_gfuncs_f77(fp);
               break;
  }
}

void gen_gfuncs_f77(FILE *fp)
{
  int rn,fn,i,j,upn,ndo,ndim;
  char ch,k;
  i_reg *ir,*irl[MAXRANK];
  gfunc_ref_list *grl;
  gfunc_tab_list *gtl,*gl;
  node *m,*d,*gf;
  index_list *o;
  grid_table *oldgrd;

  for(rn=0;rn<ninits;inits[rn++].output=0);
  for(upn=0;upn<niniters;upn++){ /* loop over initializers */
    init_header(fp,&initers[upn]);
    if(!mystrcmp(initers[upn].type->n,"auto")){ /* auto (initialization) */
      for(gtl=initers[upn].gfs;gtl!=NULL;gtl=gtl->next){ /* loop over grid functions to be initialized */
        rn=init_exists(gtl->gfunc->fname);
        if(rn==-1){
          fprintf(stderr,"Auto initializer exists for %s but no initialization.\n",gtl->gfunc->fname->n);
          fatal_error("Bad initializer declaration");
        }
        for(;rn<ninits;rn++){ /* loop over inits */
          if(inits[rn].gfunc == gtl->gfunc && !inits[rn].output){ 
            /* init for correct gfunc and this region has not been output */
            inits[rn].output=1; /* now it has */
            ndo=0;
            for(ndim=0,ir=inits[rn].reg,i=0;ir!=NULL;ndim++,ir=ir->next,i++)
              irl[i]=ir;
            for(k='i'+ndim-1,i=ndim-1;i>=0;k--,i--){
              ir=irl[i];
              if(compare_expr(ir->lower,ir->upper)){
                sprintf(forbuf,"          %c=",k);
                fort_out(fp,forbuf);
                ireg_to_string_f(fp,ir->lower,inits[rn].gfunc->grd->name);
              }else{
                ndo++;
                sprintf(forbuf,"          do %c=",k);
                fort_out(fp,forbuf);
                ireg_to_string_f(fp,ir->lower,inits[rn].gfunc->grd->name);
                fort_out(fp,", ");
                ireg_to_string_f(fp,ir->upper,inits[rn].gfunc->grd->name);
                sprintf(forbuf,", %d",ir->inc);
                fort_out(fp,forbuf);
              }
              fort_out(fp,"\n");
            }
            output_init_stat(fp,&inits[rn]);
            for(i=0;i<ndo;i++)
              fort_out(fp,"          end do\n");
          }
        }
      }
    }else if(mystrcmp(initers[upn].type->n,"stub")){
      FILE *ne;
      char nebuf[1025];
      
      ne=fopen(initers[upn].type->n,"r");
      if(ne==NULL)
        fprintf(stderr,"Unable to open file %s.\n",initers[upn].type->n);
      else{
        while(fgets(nebuf,1024,ne))
          fprintf(fp,"%s",nebuf);
        fclose(ne);
      }
    }  
    fprintf(fp,"        return\n");
    fprintf(fp,"      end\n\n");
  }
}

void gen_gfuncs_c(FILE *fp)
{
  int rn,fn,i,j,upn;
  char ch,k;
  i_reg *ir;
  gfunc_ref_list *grl;
  gfunc_tab_list *gtl,*gl;
  node *m,*d,*gf;
  index_list *o;
  
  for(rn=0;rn<ninits;inits[rn++].output=0);
  for(upn=0;upn<niniters;upn++){
    init_header(fp,&initers[upn]);
    if(!mystrcmp(initers[upn].type->n,"auto")){
      for(gtl=initers[upn].gfs;gtl!=NULL;gtl=gtl->next){
        rn=init_exists(gtl->gfunc->fname);
        if(rn==-1){
          fprintf(stderr,"Auto initializer exists for %s but no initialization.\n",gtl->gfunc->fname->n);
          fatal_error("Bad initializer declaration");
        }
        for(;rn<ninits;rn++){
          if(inits[rn].gfunc == gtl->gfunc && !inits[rn].output){
            inits[rn].output=1;
            for(k='i',ir=inits[rn].reg;ir!=NULL;k++,ir=ir->next){
              if(compare_expr(ir->lower,ir->upper)){
                fprintf(fp,"  %c=",k);
                ireg_to_string(fp,ir->lower,inits[rn].gfunc->grd->name);
                fprintf(fp,";{\n");
              }else{
                fprintf(fp,"  for(%c=",k);
                ireg_to_string(fp,ir->lower,inits[rn].gfunc->grd->name);
                if(ir->inc>0)
                  fprintf(fp,";%c<=",k);
                else fprintf(fp,";%c>=",k);
                ireg_to_string(fp,ir->upper,inits[rn].gfunc->grd->name);
                fprintf(fp,";%c+=%d){\n",k,ir->inc);
              }
            }
            output_init_stat(fp,&inits[rn]);
            for(ir=inits[rn].reg;ir!=NULL;ir=ir->next)
              fprintf(fp,"  }\n");
          }
        }
      }
    }else if(mystrcmp(initers[upn].type->n,"stub")){
      FILE *ne;
      char nebuf[1025];
      
      ne=fopen(initers[upn].type->n,"r");
      if(ne==NULL)
        fprintf(stderr,"Unable to open file %s.\n",initers[upn].type->n);
      else{
        while(fgets(nebuf,1024,ne))
          fprintf(fp,"%s",nebuf);
        fclose(ne);
      }
    }  
    fprintf(fp,"}\n\n");
  }
}

/* generate code to solve for initial data */
/* only works for 3 level grid functions */
/* 2 cases:  if 0 is the middle time level, then iterate backwards */
/* otherwise, iterate forwards */

void code_iteration(FILE *fp)
{
  update_helpers(fp);
  switch(language){
     case IDF :
     case UPF :
     case F77 :
     case F90 :
    case C   : code_iteration_c(fp);
               break;
    case ALLF: code_iteration_f77(fp);
               break;
  }
}

void code_iteration_f77(FILE *fp)
{
  int rn,fn,i,j;
  char ch;
  i_reg *ir;
  gfunc_tab_list *gtl;
  
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"!  iter_gfuncs\n");
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"      integer function iter_gfuncs(tt)\n");
  fort_out(fp,"        implicit none\n\n");
  fort_out(fp,"        real*8  tt\n");
  fort_out(fp,"        include 'sys_param.inc'\n");
  fort_out(fp,"        include 'gfuni0.inc'\n");
  fort_out(fp,"        include 'other_glbs.inc'\n");
  fort_out(fp,"        include 'globals.inc'\n\n");
  fort_out(fp,"        real*8  t,u,d,rdvupdatemean\n");
  fort_out(fp,"        integer i,j,k,step\n\n");
  fprintf(fp,"\n");
  fort_out(fp,"        t=tt\n");
  sprintf(forbuf,"        d=%s\n",coord_difs[0][0]->n);
  fort_out(fp,forbuf);
  sprintf(forbuf,"        %s=-%s/2\n",coord_difs[0][0]->n,coord_difs[0][0]->n);
  fort_out(fp,forbuf);
  /* initial guess */
  for(rn=0;rn<nupdates;rn++)
    for(gtl=updates[rn].gfs;gtl!=NULL;gtl=gtl->next){
         if(gtl->gfunc->neediditer){
        sprintf(forbuf,"        call rdvcpy(q(ptrs(gf_st(%d)+3)),",
              gfunc_to_index(gtl->gfunc)+1);
        fort_out(fp,forbuf);
        sprintf(forbuf,"q(ptrs(gf_st(%d)+2)),",gfunc_to_index(gtl->gfunc)+1);
        fort_out(fp,forbuf);
        grid_size(fp,gtl->gfunc->grd);
        fort_out(fp,")\n");
         }
      if(gtl->gfunc->ntlevs>1){
        sprintf(forbuf,"        call rdvcpy(q(ptrs(gf_st(%d)+1)),",
              gfunc_to_index(gtl->gfunc)+1);
        fort_out(fp,forbuf);
        sprintf(forbuf,"q(ptrs(gf_st(%d)+2)),",gfunc_to_index(gtl->gfunc)+1);
        fort_out(fp,forbuf);
        grid_size(fp,gtl->gfunc->grd);
        fort_out(fp,")\n");
      }
    }
  fort_out(fp,"        step=0\n");
  fort_out(fp,"100     continue\n");
  fort_out(fp,"          step=step+1\n");
  fort_out(fp,"          u=");
  fort_out_const(forbuf,0.0);
  fort_out(fp,forbuf);
  fort_out(fp,"\n");
  for(rn=0;rn<nupdates;rn++)
    for(gtl=updates[rn].gfs;gtl!=NULL;gtl=gtl->next){
      if(gtl->gfunc->neediditer){
        sprintf(forbuf,"          u=u+rdvupdatemean(q(ptrs(gf_st(%d)+2)),",
                gfunc_to_index(gtl->gfunc)+1);
        fort_out(fp,forbuf);
        sprintf(forbuf,"q(ptrs(gf_st(%d)+%d)),",
              gfunc_to_index(gtl->gfunc)+1,1);
        fort_out(fp,forbuf);
        sprintf(forbuf,"q(ptrs(gf_st(%d)+3)),",gfunc_to_index(gtl->gfunc)+1);
        fort_out(fp,forbuf);
        grid_size(fp,gtl->gfunc->grd);
        fort_out(fp,")\n");
      }
    }
  fort_out(fp,"          call take_step(t)\n");
  fort_out(fp,"        if((u .gt. epsiterid");
  fort_out(fp," .AND. step .lt. maxstepid)");
  fort_out(fp,".OR. step .lt. 2) then\n");
  fort_out(fp,"          goto 100\n");
  fort_out(fp,"        end if\n");

  /* swap time levels */
  fort_out(fp,"        call swap_top()\n");
  fort_out(fp,"        if(step .eq. maxstepid) then\n");
  fort_out(fp,"          write (*,*) ");
  fort_out(fp,"'Initialization did not converge within ',");
  fort_out(fp,"maxstepid,");
  fort_out(fp,"' iterations.'\n");
  fort_out(fp,"        else\n");
  fort_out(fp,"          write (*,*) ");
  fort_out(fp,"'Initialization converged in ',");
  fort_out(fp,"step,");
  fort_out(fp,"' iterations.'\n");
  fort_out(fp,"        end if\n");
  sprintf(forbuf,"        %s=d\n",coord_difs[0][0]->n);
  fort_out(fp,forbuf);
  fort_out(fp,"        iter_gfuncs = step\n");
  fort_out(fp,"        return\n");
  fort_out(fp,"      end\n");
}

void update_iter_header_args(FILE *fp)
{
  gfunc_ref_list *gl;
  coord_list *cl;
  i_reg *ir;
  int i,j,rn,fl,used_time=0;
  gfunc_tab_list *gt;
  param_ref_list *pr;
  gfunc_table *gf;
  work_list *wl;

  for(i=0;i<nresids;){
     gf=resids[i].gfunc;
     if(resids[i].eval){
        switch(resids[i].gfunc->type){
          case INT :
             fprintf(fp,"int *%s_res,",resids[i].gfunc->fname->n);
             break;
          case FLOAT :
             fprintf(fp,"double *%s_res,",resids[i].gfunc->fname->n);
             break;
        }
     }
    while(i<nresids && gf==resids[i].gfunc)
      i++;
  }
  for(gl=update_glob_gf;gl!=NULL;gl=gl->next){
    switch(gl->gfunc->type){
      case INT   : fprintf(fp,"int *");
                   break;
      case FLOAT : fprintf(fp,"double *");
                   break;
    }
    fprintf(fp,"%s",gl->gfunc->fname->n);
    gfunc_suffix(fp,gl->toff);
    if((gl->next && gl->next->gfunc->grd != gl->gfunc->grd) || !gl->next){
      for(i=0,cl=gl->gfunc->grd->clst;i<gl->gfunc->grd->reg.rank;i++,cl=cl->next){
        fprintf(fp,",int %s_N%s",gl->gfunc->grd->name->n,cl->name->n);
      }
    }
    if(gl->next) fprintf(fp,",");
  }
  for(cl=update_glob_crds;cl!=NULL;cl=cl->next){
    if(is_space(cl->name))
      fprintf(fp,",double *%s",cl->name->n);
    else{
        fprintf(fp,",double %s",cl->name->n);
        if(cl->name==coords[0].c_names->name)
          used_time=1;
    }
  }
  for(cl=update_glob_cdifs;cl!=NULL;cl=cl->next){
    fprintf(fp,",double %s",cl->name->n);
  }
  for(pr=update_glob_par;pr!=NULL;pr=pr->next){
    switch(pr->par->type){
      case  INT    :  fprintf(fp,",int ");
                    break;
      case  FLOAT  :  fprintf(fp,",double ");
                    break;
    }
    if(vsize(pr->par->size)>1)
      fprintf(fp,"*");
    fprintf(fp,"%s",pr->par->name->n);
  }
   if(!used_time)
     fprintf(fp,",double %s",coords[0].c_names->name->n);
  for(wl=static_work;wl!=NULL;wl=wl->next){
     fprintf(fp,",double *work%d,int nwork%d",wl->work->num,wl->work->num);
  }
}

void update_iter_call_args(FILE *fp)
{
  gfunc_ref_list *gl;
  coord_list *cl;
  i_reg *ir;
  int i,j,rn,fl,used_time=0;
  gfunc_tab_list *gt;
  param_ref_list *pr;
  gfunc_table *gf;
  work_list *wl;

  for(i=0;i<nresids;){
     gf=resids[i].gfunc;
     if(resids[i].eval){
       fprintf(fp,"%s_res,",resids[i].gfunc->fname->n);
     }
    while(i<nresids && gf==resids[i].gfunc)
      i++;
  }
  
  for(gl=update_glob_gf;gl!=NULL;gl=gl->next){
    fprintf(fp,"%s",gl->gfunc->fname->n);
    gfunc_suffix(fp,gl->toff);
    if((gl->next && gl->next->gfunc->grd != gl->gfunc->grd) || !gl->next){
      for(i=0;i<gl->gfunc->grd->reg.rank;i++){
        fprintf(fp,",lats[%d].shape[%d]",gptr_to_index(gl->gfunc->grd),i);
      }
    }
    if(gl->next) fprintf(fp,",");
  }
  for(cl=update_glob_crds;cl!=NULL;cl=cl->next){
    if(is_space(cl->name))
      fprintf(fp,",%s",cl->name->n);
    else{
        fprintf(fp,",%s",cl->name->n);
        if(cl->name==coords[0].c_names->name)
          used_time=1;
    }
  }
  for(cl=update_glob_cdifs;cl!=NULL;cl=cl->next){
    fprintf(fp,",%s",cl->name->n);
  }
  for(pr=update_glob_par;pr!=NULL;pr=pr->next){
    fprintf(fp,",%s",pr->par->name->n);
  }
   if(!used_time)
     fprintf(fp,",%s",coords[0].c_names->name->n);
  for(wl=static_work;wl!=NULL;wl=wl->next){
     fprintf(fp,",work%d,nwork%d",wl->work->num,wl->work->num);
  }
}

/* iteration only works with 3-level schemes when the 0th level is initialized */
void code_iteration_c(FILE *fp)
{
  int rn,fn,i;
  char ch;
  i_reg *ir;
  gfunc_tab_list *gtl;
  
  fprintf(fp,"int iter_gfuncs(int *rnpldone, int maxstepid, double epsiterid,");
  update_iter_header_args(fp);
  fprintf(fp,")\n{\n");
  fprintf(fp,"  int i,j,k,step;\n");
  fprintf(fp,"  double rnplu;\n\n");
  fprintf(fp,"  %s/=-2;\n",coord_difs[0][0]->n);
  /* initial guess */
  for(rn=0;rn<nupdates;rn++)
    for(gtl=updates[rn].gfs;gtl!=NULL;gtl=gtl->next){
      offset_type *o;
         if(gtl->gfunc->neediditer){
            o=gtl->gfunc->tlev->next; /* middle (0) level */
            fprintf(fp,"  rdvcpy(%s",gtl->gfunc->fname->n);
            gfunc_suffix(fp,o->next->offset);
            fprintf(fp,",%s",gtl->gfunc->fname->n);
            gfunc_suffix(fp,o->offset);
            fprintf(fp,",");
        grid_size(fp,gtl->gfunc->grd);
        fprintf(fp,");\n");
         }
      if(gtl->gfunc->ntlevs>1){
        o=gtl->gfunc->tlev;
        fprintf(fp,"  rdvcpy(%s",gtl->gfunc->fname->n);
        gfunc_suffix(fp,o->offset);
        fprintf(fp,",%s",gtl->gfunc->fname->n);
        gfunc_suffix(fp,o->next->offset);
        fprintf(fp,",");
        grid_size(fp,gtl->gfunc->grd);
        fprintf(fp,");\n");
      }
    }
  fprintf(fp,"  step=0;\n  do{\n    step++;\n");
  fprintf(fp,"    rnplu=0.0;\n");
  for(rn=0;rn<nupdates;rn++)
    for(gtl=updates[rn].gfs;gtl!=NULL;gtl=gtl->next){
      if(gtl->gfunc->neediditer){
        offset_type *o;
        fprintf(fp,"    rnplu+=rdvupdatemean(%s",gtl->gfunc->fname->n);
        o=gtl->gfunc->tlev->next;
        gfunc_suffix(fp,o->offset);
        fprintf(fp,",%s",gtl->gfunc->fname->n);
        o=gtl->gfunc->tlev;
        gfunc_suffix(fp,o->offset);
        fprintf(fp,",%s",gtl->gfunc->fname->n);
        o=gtl->gfunc->tlev->next->next;
        gfunc_suffix(fp,o->offset);
        fprintf(fp,",");
        grid_size(fp,gtl->gfunc->grd);
        fprintf(fp,");\n");
      }
    }
  fprintf(fp,"    ");
  take_step_call(fp);
  fprintf(fp,"  }while((rnplu>epsiterid && step<maxstepid) || step<2);\n");

  fprintf(fp,"  if(step==maxstepid)\n");
  fprintf(fp,"    fprintf(stdout,\"Initialization did not converge within %%d iterations.\\n\",maxstepid);\n");
   fprintf(fp,"  else\n");
   fprintf(fp,"    fprintf(stdout,\"Initialization converged in %%d iterations.\\n\",step);\n");
  fprintf(fp,"  return(step);\n");
  fprintf(fp,"}\n\n");
}

void make_c_header(FILE *fp)
{
  fprintf(fp,"/* This code was generated by rnpl, a numerical programming language */\n");
  fprintf(fp,"/* copyright (c) 1994-1998 by Robert L. Marsa and Matthew W. Choptuik */\n");
  fprintf(fp,"\n");
  fprintf(fp,"#include <stdlib.h>\n");
  fprintf(fp,"#include <stdio.h>\n");
  fprintf(fp,"#include <math.h>\n");
  fprintf(fp,"#include <string.h>\n");
  fprintf(fp,"#include <time.h>\n");
  fprintf(fp,"#include <signal.h>\n");
  fprintf(fp,"#include <sdf.h>\n");
  fprintf(fp,"#include <librnpl.h>\n");
  if(parallel==TRIV)
     fprintf(fp,"#include <mpi.h>\n");
  fprintf(fp,"\n");
  fprintf(fp,"#ifndef TRUE\n");
  fprintf(fp,"#define TRUE 1\n");
  fprintf(fp,"#endif\n");
  fprintf(fp,"#ifndef FALSE\n");
  fprintf(fp,"#define FALSE 0\n");
  fprintf(fp,"#endif\n");
  fprintf(fp,"\n");
}

void make_f77_header(FILE *fp)
{
  fprintf(fp,"!   This code was generated by rnpl, a numerical programming language \n");
  fprintf(fp,"!   copyright (c) 1994-1998 by Robert L. Marsa and Matthew W. Choptuik \n");
  fprintf(fp,"c-----------------------------------------------------------------------\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c     M  E  M  O  R  Y        M  A  N  A  G  E  M  E  N  T\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c                    R  O  U  T  I  N  E  S\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c-----------------------------------------------------------------------\n");
  fprintf(fp,"c     These routines, in conjunction with the file 'mm.inc' reproduced\n");
  fprintf(fp,"c     here:\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c        integer       maxblk\n");
  fprintf(fp,"c        parameter     ( maxblk = 1 000 )\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c        integer       mmmloc(0:maxblk),\n");
  fprintf(fp,"c    *                 mmsize(0:maxblk),\n");
  fprintf(fp,"c    *                 mmnext(0:maxblk)\n");
  fprintf(fp,"c        logical       mfree(0:maxblk)\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c        common        / commm / mmmloc, mmsize, mmnext, mfree\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c     constitute a FORTRAN implementation of the ``First-fit''\n");
  fprintf(fp,"c     memory management scheme described in \n");
  fprintf(fp,"c \n");
  fprintf(fp,"c     D. Knuth\n");
  fprintf(fp,"c     The Art of Computer Programming\n");
  fprintf(fp,"c     Vol. 1, Fundamental Algorithms\n");
  fprintf(fp,"c     Section 2.5: Dynamic Storage Allocation\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c     Invoking routines are responsible for declaring actual storage \n");
  fprintf(fp,"c     array (arena) from which blocks are allocated/deallocated.\n");
  fprintf(fp,"c    \n");
  fprintf(fp,"c     Example arena declaration:\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c        integer      memsiz\n");
  fprintf(fp,"c        parameter  ( memsiz = 1 000 000 )\n");
  fprintf(fp,"c        real*8       q(memsiz)\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c-----------------------------------------------------------------------\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c     Sample usage (given above declarations)\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c     Initialize memory management structure ...\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c        integer    mmaloc,   mmdeal\n");
  fprintf(fp,"c        integer    ptr1,     ptr2\n");
  fprintf(fp,"c \n");
  fprintf(fp,"c        call mmini(memsiz)\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c     Allocate two blocks of storage ...\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c        ptr1 = mmaloc(1000)\n");
  fprintf(fp,"c        if( ptr1 .lt. 0 ) then\n");
  fprintf(fp,"c           write(*,*) 'Unable to allocate block of size 1000'\n");
  fprintf(fp,"c           stop\n");
  fprintf(fp,"c        end if\n");
  fprintf(fp,"c        ptr2 = mmaloc(1000)\n");
  fprintf(fp,"c        if( ptr2 .lt. 0 ) then\n");
  fprintf(fp,"c           write(*,*) 'Unable to allocate block of size 1000'\n");
  fprintf(fp,"c           stop\n");
  fprintf(fp,"c        end if\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c     ... do something with the blocks ...\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c        call subroutine_which_wants_2_vectors(q(ptr1),q(ptr2),1000)\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c     ... and free them. Note that block size must be supplied to \n");
  fprintf(fp,"c     deallocating routine.\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c        ptr1 = mmdeal(ptr1,1000)\n");
  fprintf(fp,"c        ptr2 = mmdeal(ptr2,1000)\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c-----------------------------------------------------------------------\n");
  fprintf(fp,"\n");
  fprintf(fp,"c=======================================================================\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c  subroutine  mmini(memsiz)\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c  I: memsiz             integer        Arena size\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c  O: None\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c     Initializes memory management structure.\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c=======================================================================\n");
  fprintf(fp," \n");
  fprintf(fp,"      subroutine mmini(memsiz)\n");
  fprintf(fp," \n");
  fprintf(fp,"         implicit      none\n");
  fprintf(fp," \n");
  fprintf(fp,"C-----------------------------------------------------------------------\n");
  fprintf(fp,"C\n");
  fprintf(fp,"C     M  E  M  O  R  Y      M  A  N  A  G  E  M  E  N  T\n");
  fprintf(fp,"C\n");
  fprintf(fp,"C                  S  T  R  U  C  T  U  R  E\n");
  fprintf(fp,"C\n");
  fprintf(fp,"C-----------------------------------------------------------------------\n");
  fprintf(fp,"C\n");
  fprintf(fp,"      integer       maxblk\n");
  fprintf(fp,"      parameter     ( maxblk = 1 000 )\n");
  fprintf(fp,"C\n");
  fprintf(fp,"      integer       mmmloc(0:maxblk),\n");
  fprintf(fp,"     *              mmsize(0:maxblk),\n");
  fprintf(fp,"     *              mmnext(0:maxblk)\n");
  fprintf(fp,"      logical       mfree(0:maxblk)\n");
  fprintf(fp,"C\n");
  fprintf(fp,"      common        / commm / mmmloc, mmsize, mmnext, mfree\n");
  fprintf(fp,"C\n");
  fprintf(fp,"C----------------------------------------------------------------------\n");
  fprintf(fp," \n");
  fprintf(fp,"         integer       memsiz\n");
  fprintf(fp," \n");
  fprintf(fp,"         call l_ivls(mmmloc(0),0,maxblk+1)\n");
  fprintf(fp,"         call l_ivls(mmsize(0),0,maxblk+1)\n");
  fprintf(fp,"         call l_ivls(mmnext(0),0,maxblk+1)\n");
  fprintf(fp,"         mmnext(0) = 1\n");
  fprintf(fp,"         mmmloc(1) = 1\n");
  fprintf(fp,"         mmsize(1) = memsiz\n");
  fprintf(fp," \n");
  fprintf(fp,"         return\n");
  fprintf(fp," \n");
  fprintf(fp,"      end\n");
  fprintf(fp," \n");
  fprintf(fp,"c=======================================================================\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c  integer function mmfree()\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c  I: None\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c  O: None\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c     Returns first available location in list of memory block nodes,\n");
  fprintf(fp,"c     -1 if none available.\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c=======================================================================\n");
  fprintf(fp," \n");
  fprintf(fp,"      integer function mmfree()\n");
  fprintf(fp," \n");
  fprintf(fp,"         implicit     none\n");
  fprintf(fp," \n");
  fprintf(fp,"C-----------------------------------------------------------------------\n");
  fprintf(fp,"C\n");
  fprintf(fp,"C     M  E  M  O  R  Y      M  A  N  A  G  E  M  E  N  T\n");
  fprintf(fp,"C\n");
  fprintf(fp,"C                  S  T  R  U  C  T  U  R  E\n");
  fprintf(fp,"C\n");
  fprintf(fp,"C-----------------------------------------------------------------------\n");
  fprintf(fp,"C\n");
  fprintf(fp,"      integer       maxblk\n");
  fprintf(fp,"      parameter     ( maxblk = 1 000 )\n");
  fprintf(fp,"C\n");
  fprintf(fp,"      integer       mmmloc(0:maxblk),\n");
  fprintf(fp,"     *              mmsize(0:maxblk),\n");
  fprintf(fp,"     *              mmnext(0:maxblk)\n");
  fprintf(fp,"      logical       mfree(0:maxblk)\n");
  fprintf(fp,"C\n");
  fprintf(fp,"      common        / commm / mmmloc, mmsize, mmnext, mfree\n");
  fprintf(fp,"C\n");
  fprintf(fp,"C----------------------------------------------------------------------\n");
  fprintf(fp," \n");
  fprintf(fp,"         integer      p,     q\n");
  fprintf(fp," \n");
  fprintf(fp,"         do mmfree = 0 , maxblk\n");
  fprintf(fp,"            mfree(mmfree) = .true.\n");
  fprintf(fp,"         end do\n");
  fprintf(fp,"         q = 0\n");
  fprintf(fp," 200     continue\n");
  fprintf(fp,"            p = mmnext(q)\n");
  fprintf(fp,"            if( p .eq. 0 ) go to 300\n");
  fprintf(fp,"            mfree(p) = .false.\n");
  fprintf(fp,"            q = p\n");
  fprintf(fp,"         go to 200\n");
  fprintf(fp," 300     continue\n");
  fprintf(fp,"         do mmfree = 1 , maxblk\n");
  fprintf(fp,"            if( mfree(mmfree) ) return\n");
  fprintf(fp,"         end do\n");
  fprintf(fp,"         mmfree = -1\n");
  fprintf(fp," \n");
  fprintf(fp,"         return\n");
  fprintf(fp," \n");
  fprintf(fp,"      end\n");
  fprintf(fp," \n");
  fprintf(fp,"c=======================================================================\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c  integer function mmaloc(reqsiz)\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c  I: reqsiz             integer        Requested block size.\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c  O: None\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c     Allocates memory block of size REQSIZ, returning pointer to first\n");
  fprintf(fp,"c     location, or -1 if unable to perform allocation.\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c=======================================================================\n");
  fprintf(fp," \n");
  fprintf(fp,"      integer function mmaloc(reqsiz)\n");
  fprintf(fp," \n");
  fprintf(fp,"         implicit     none\n");
  fprintf(fp," \n");
  fprintf(fp,"C-----------------------------------------------------------------------\n");
  fprintf(fp,"C\n");
  fprintf(fp,"C     M  E  M  O  R  Y      M  A  N  A  G  E  M  E  N  T\n");
  fprintf(fp,"C\n");
  fprintf(fp,"C                  S  T  R  U  C  T  U  R  E\n");
  fprintf(fp,"C\n");
  fprintf(fp,"C-----------------------------------------------------------------------\n");
  fprintf(fp,"C\n");
  fprintf(fp,"      integer       maxblk\n");
  fprintf(fp,"      parameter     ( maxblk = 1 000 )\n");
  fprintf(fp,"C\n");
  fprintf(fp,"      integer       mmmloc(0:maxblk),\n");
  fprintf(fp,"     *              mmsize(0:maxblk),\n");
  fprintf(fp,"     *              mmnext(0:maxblk)\n");
  fprintf(fp,"      logical       mfree(0:maxblk)\n");
  fprintf(fp,"C\n");
  fprintf(fp,"      common        / commm / mmmloc, mmsize, mmnext, mfree\n");
  fprintf(fp,"C\n");
  fprintf(fp,"C----------------------------------------------------------------------\n");
  fprintf(fp," \n");
  fprintf(fp,"         integer      k, p, q, reqsiz\n");
  fprintf(fp," \n");
  fprintf(fp,"         q = 0\n");
  fprintf(fp," 100     continue\n");
  fprintf(fp,"            p = mmnext(q)\n");
  fprintf(fp,"            if( p .eq. 0 ) then\n");
  fprintf(fp,"               mmaloc = -1\n");
  fprintf(fp,"               return\n");
  fprintf(fp,"            end if\n");
  fprintf(fp,"            if( mmsize(p) .ge. reqsiz ) then\n");
  fprintf(fp,"               k = mmsize(p) - reqsiz\n");
  fprintf(fp,"               if( k .eq. 0 ) then\n");
  fprintf(fp,"                  mmnext(q) = mmnext(p)\n");
  fprintf(fp,"               else\n");
  fprintf(fp,"                  mmsize(p) = k\n");
  fprintf(fp,"               end if\n");
  fprintf(fp,"               mmaloc = mmmloc(p) + k\n");
  fprintf(fp,"               return\n");
  fprintf(fp,"            end if\n");
  fprintf(fp,"            q = mmnext(q)\n");
  fprintf(fp,"         go to 100\n");
  fprintf(fp," \n");
  fprintf(fp,"         end\n");
  fprintf(fp," \n");
  fprintf(fp," \n");
  fprintf(fp,"c=======================================================================\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c  integer function mmdeal(p0loc,p0size)\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c  I: p0loc              integer        Pointer to block to be freed.\n");
  fprintf(fp,"c     p0size             integer        Size of block to be freed.\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c  O: None\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c        Returns block of storage of length P0SIZE, which begins at\n");
  fprintf(fp,"c        location P0LOC to free storage list.\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c=======================================================================\n");
  fprintf(fp," \n");
  fprintf(fp,"      integer function mmdeal(p0loc,p0size)\n");
  fprintf(fp," \n");
  fprintf(fp,"         implicit    none\n");
  fprintf(fp," \n");
  fprintf(fp,"C-----------------------------------------------------------------------\n");
  fprintf(fp,"C\n");
  fprintf(fp,"C     M  E  M  O  R  Y      M  A  N  A  G  E  M  E  N  T\n");
  fprintf(fp,"C\n");
  fprintf(fp,"C                  S  T  R  U  C  T  U  R  E\n");
  fprintf(fp,"C\n");
  fprintf(fp,"C-----------------------------------------------------------------------\n");
  fprintf(fp,"C\n");
  fprintf(fp,"      integer       maxblk\n");
  fprintf(fp,"      parameter     ( maxblk = 1 000 )\n");
  fprintf(fp,"C\n");
  fprintf(fp,"      integer       mmmloc(0:maxblk),\n");
  fprintf(fp,"     *              mmsize(0:maxblk),\n");
  fprintf(fp,"     *              mmnext(0:maxblk)\n");
  fprintf(fp,"      logical       mfree(0:maxblk)\n");
  fprintf(fp,"C\n");
  fprintf(fp,"      common        / commm / mmmloc, mmsize, mmnext, mfree\n");
  fprintf(fp,"C\n");
  fprintf(fp,"C----------------------------------------------------------------------\n");
  fprintf(fp," \n");
  fprintf(fp,"         integer     mmfree\n");
  fprintf(fp,"         integer     p, p0, p0loc, p0size, q\n");
  fprintf(fp," \n");
  fprintf(fp,"         p0 = mmfree()\n");
  fprintf(fp,"         if( p0 .lt. 0 ) then\n");
  fprintf(fp,"            mmdeal = p0\n");
  fprintf(fp,"            return\n");
  fprintf(fp,"         else\n");
  fprintf(fp,"            mmmloc(p0) = p0loc\n");
  fprintf(fp,"            mmsize(p0) = p0size\n");
  fprintf(fp,"         end if\n");
  fprintf(fp,"         q = 0\n");
  fprintf(fp," 100     continue\n");
  fprintf(fp,"            p = mmnext(q)\n");
  fprintf(fp,"            if( mmmloc(p) .gt. mmmloc(p0)  .or.  p .eq. 0 ) then\n");
  fprintf(fp,"               if( mmmloc(p0) + mmsize(p0) .eq. mmmloc(p) ) then\n");
  fprintf(fp,"                  mmnext(p0) = mmnext(p)\n");
  fprintf(fp,"                  mmsize(p0) = mmsize(p0) + mmsize(p)\n");
  fprintf(fp,"               else\n");
  fprintf(fp,"                  mmnext(p0) = p\n");
  fprintf(fp,"               end if\n");
  fprintf(fp,"               if( mmmloc(q) + mmsize(q) .eq. mmmloc(p0) ) then\n");
  fprintf(fp,"                  mmnext(q) = mmnext(p0)\n");
  fprintf(fp,"                  mmsize(q) = mmsize(q) + mmsize(p0)\n");
  fprintf(fp,"               else\n");
  fprintf(fp,"                  mmnext(q) = p0\n");
  fprintf(fp,"               end if\n");
  fprintf(fp,"               mmdeal = 0\n");
  fprintf(fp,"               return\n");
  fprintf(fp,"            end if\n");
  fprintf(fp,"            q = p\n");
  fprintf(fp,"         go to 100\n");
  fprintf(fp," \n");
  fprintf(fp,"      end\n");
  fprintf(fp," \n");
  fprintf(fp,"c=======================================================================\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c  subroutine mmdump(unit)\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c  I: unit               integer        Logical unit for dump.\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c  O: None\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c     Dumps map of free list on UNIT.\n");
  fprintf(fp,"c\n");
  fprintf(fp,"c=======================================================================\n");
  fprintf(fp," \n");
  fprintf(fp,"      subroutine mmdump(unit)\n");
  fprintf(fp," \n");
  fprintf(fp,"         implicit none\n");
  fprintf(fp," \n");
  fprintf(fp,"C-----------------------------------------------------------------------\n");
  fprintf(fp,"C\n");
  fprintf(fp,"C     M  E  M  O  R  Y      M  A  N  A  G  E  M  E  N  T\n");
  fprintf(fp,"C\n");
  fprintf(fp,"C                  S  T  R  U  C  T  U  R  E\n");
  fprintf(fp,"C\n");
  fprintf(fp,"C-----------------------------------------------------------------------\n");
  fprintf(fp,"C\n");
  fprintf(fp,"      integer       maxblk\n");
  fprintf(fp,"      parameter     ( maxblk = 1 000 )\n");
  fprintf(fp,"C\n");
  fprintf(fp,"      integer       mmmloc(0:maxblk),\n");
  fprintf(fp,"     *              mmsize(0:maxblk),\n");
  fprintf(fp,"     *              mmnext(0:maxblk)\n");
  fprintf(fp,"      logical       mfree(0:maxblk)\n");
  fprintf(fp,"C\n");
  fprintf(fp,"      common        / commm / mmmloc, mmsize, mmnext, mfree\n");
  fprintf(fp,"C\n");
  fprintf(fp,"C----------------------------------------------------------------------\n");
  fprintf(fp," \n");
  fprintf(fp,"         integer      n, p, unit\n");
  fprintf(fp," \n");
  fprintf(fp,"         write(unit,100)\n");
  fprintf(fp," 100     FORMAT(//T10,' <<<     Free Block Structure.     >>>'//\n");
  fprintf(fp,"     *            T11,'Block',  T29,'Locations'/)\n");
  fprintf(fp,"         p = 0\n");
  fprintf(fp,"         n = 0\n");
  fprintf(fp," 200     continue\n");
  fprintf(fp,"            p = mmnext(p)\n");
  fprintf(fp,"            if( p .eq. 0 ) then\n");
  fprintf(fp,"               return\n");
  fprintf(fp,"            else\n");
  fprintf(fp,"               n = n + 1\n");
  fprintf(fp,"               write(unit,300) n, mmmloc(p), mmmloc(p) + mmsize(p) - 1\n");
  fprintf(fp," 300           FORMAT(T12,I3,T25,I7,' ... ',I8)\n");
  fprintf(fp,"            end if\n");
  fprintf(fp,"         go to 200\n");
  fprintf(fp," \n");
  fprintf(fp,"      end\n");
  fprintf(fp,"\n");
  fprintf(fp,"!-----------------------------------------------------------------------\n");
  fprintf(fp,"!     Front ends to latinfo for extracting:\n");
  fprintf(fp,"!\n");
  fprintf(fp,"!     getlatid(ilat):    Lattice id of ilat'th lattice\n");
  fprintf(fp,"!     getlatcs(ilat):    Coordinate system of ilat'th lattice (0..)\n");
  fprintf(fp,"!     getrank (ilat):    Rank of ilat'th lat\n");
  fprintf(fp,"!     getpshape(ilat):   Pointer into PTRS for shape of lat\n");
  fprintf(fp,"!                           ptrs(getpshape(ilat) - 1 + j) := shape(j)\n");
  fprintf(fp,"!     getshape(ilat,j):  j'th dim (element of shape) of ilat'th lat\n");
  fprintf(fp,"!                           ptrs(getpshape(ilat) - 1 + j) := shape(j)\n");
  fprintf(fp,"!     getpbounds(ilat):  Pointer into PTRS for bounds of lat\n");
  fprintf(fp,"!                           ptrs(getpbounds(ilat) - 1 + j) := bounds(j)\n");
  fprintf(fp,"!     getbounds(ilat,j): j'th component of bounds\n");
  fprintf(fp,"!     getpcoord(ilat,j)  Pointer into Q for j'th coords. of ilat'th lat\n");
  fprintf(fp,"!     getsize(ilat):     Size of lattice (product reduction of shape)\n");
  fprintf(fp,"!-----------------------------------------------------------------------\n");
  fprintf(fp,"      integer function getnlat()\n");
  fprintf(fp,"         implicit      none\n");
  fprintf(fp,"         integer       latinfo\n");
  fprintf(fp,"         getnlat = latinfo(0,0,0)\n");
  fprintf(fp,"         return\n");
  fprintf(fp,"      end\n");
  fprintf(fp,"\n");
  fprintf(fp,"      integer function getlatcs(ilat)\n");
  fprintf(fp,"         implicit      none\n");
  fprintf(fp,"         integer       latinfo\n");
  fprintf(fp,"         integer       ilat\n");
  fprintf(fp,"         getlatcs = latinfo(ilat,0,9)\n");
  fprintf(fp,"         return\n");
  fprintf(fp,"      end\n");
  fprintf(fp,"\n");
  fprintf(fp,"      integer function getlatid(ilat)\n");
  fprintf(fp,"         implicit      none\n");
  fprintf(fp,"         integer       latinfo\n");
  fprintf(fp,"         integer       ilat\n");
  fprintf(fp,"         getlatid = latinfo(ilat,0,1)\n");
  fprintf(fp,"         return\n");
  fprintf(fp,"      end\n");
  fprintf(fp,"\n");
  fprintf(fp,"      integer function getrank(ilat)\n");
  fprintf(fp,"         implicit      none\n");
  fprintf(fp,"         integer       latinfo\n");
  fprintf(fp,"         integer       ilat\n");
  fprintf(fp,"         getrank  = latinfo(ilat,0,2)\n");
  fprintf(fp,"         return\n");
  fprintf(fp,"      end\n");
  fprintf(fp,"\n");
  fprintf(fp,"      integer function getpshape(ilat)\n");
  fprintf(fp,"         implicit      none\n");
  fprintf(fp,"         integer       latinfo\n");
  fprintf(fp,"         integer       ilat\n");
  fprintf(fp,"         getpshape = latinfo(ilat,0,3)\n");
  fprintf(fp,"         return\n");
  fprintf(fp,"      end\n");
  fprintf(fp,"\n");
  fprintf(fp,"      integer function getshape(ilat,j)\n");
  fprintf(fp,"         implicit      none\n");
  fprintf(fp,"         integer       latinfo\n");
  fprintf(fp,"         integer       ilat,    j\n");
  fprintf(fp,"         getshape = latinfo(ilat,j,4)\n");
  fprintf(fp,"         return\n");
  fprintf(fp,"      end\n");
  fprintf(fp,"\n");
  fprintf(fp,"      integer function getpbounds(ilat)\n");
  fprintf(fp,"         implicit      none\n");
  fprintf(fp,"         integer       latinfo\n");
  fprintf(fp,"         integer       ilat\n");
  fprintf(fp,"         getpbounds = latinfo(ilat,0,7)\n");
  fprintf(fp,"         return\n");
  fprintf(fp,"      end\n");
  fprintf(fp,"\n");
  fprintf(fp,"      integer function getbounds(ilat,j)\n");
  fprintf(fp,"         implicit      none\n");
  fprintf(fp,"         integer       latinfo\n");
  fprintf(fp,"         integer       ilat,    j\n");
  fprintf(fp,"         getbounds = latinfo(ilat,j,8)\n");
  fprintf(fp,"         return\n");
  fprintf(fp,"      end\n");
  fprintf(fp,"      \n");
  fprintf(fp,"      integer function getpcoord(ilat,j)\n");
  fprintf(fp,"         implicit      none\n");
  fprintf(fp,"         integer       latinfo\n");
  fprintf(fp,"         integer       ilat,    j\n");
  fprintf(fp,"         getpcoord = latinfo(ilat,j,5)\n");
  fprintf(fp,"         return\n");
  fprintf(fp,"      end\n");
  fprintf(fp,"\n");
  fprintf(fp,"      integer function getsize(ilat)\n");
  fprintf(fp,"         implicit      none\n");
  fprintf(fp,"         integer       latinfo\n");
  fprintf(fp,"         integer       ilat\n");
  fprintf(fp,"         getsize = latinfo(ilat,0,6)\n");
  fprintf(fp,"         return\n");
  fprintf(fp,"      end\n");
  fprintf(fp,"\n");
  fprintf(fp,"!-----------------------------------------------------------------------\n");
  fprintf(fp,"!     Main decoding routine\n");
  fprintf(fp,"!\n");
  fprintf(fp,"!     Requires 'l_ivprod' for product reduction of integer vector.\n");
  fprintf(fp,"!\n");
  fprintf(fp,"!     option = 0:   Number of lattices\n");
  fprintf(fp,"!     option = 1:   Lattice id\n");
  fprintf(fp,"!     option = 2:   Rank of ilat'th lattice\n");
  fprintf(fp,"!     option = 3:   Pointer into ptrs for shape of ilat'th lattice\n");
  fprintf(fp,"!     option = 4:   j'th dim of ilat'th lattice\n");
  fprintf(fp,"!     option = 5:   Pointer into q for coordinates of j'th dim of\n");
  fprintf(fp,"!                   ilat'th lattice\n");
  fprintf(fp,"!     option = 6:   Size of lattice (product reduction of shape)\n");
  fprintf(fp,"!     option = 7:   Pointer into ptrs for bounds of ilat'th lattice\n");
  fprintf(fp,"!     option = 8:   j'th bound of ilat'th lattice\n");
  fprintf(fp,"!     option = 9:   Coordinate system\n");
  fprintf(fp,"!-----------------------------------------------------------------------\n");
  fprintf(fp,"      integer function latinfo(ilat,j,option)\n");
  fprintf(fp,"         implicit      none\n");
  fprintf(fp,"\n");
  fprintf(fp,"         include      'sys_param.inc'\n");
  fprintf(fp,"         include      'gfuni0.inc'\n");
  fprintf(fp,"\n");
  fprintf(fp,"         integer       l_ivprod\n");
  fprintf(fp,"\n");
  fprintf(fp,"         integer       ilat,       j,       option, cs\n");
  fprintf(fp,"         integer       nlat,       base,    latid,  rank\n");
  fprintf(fp,"         integer  shape, bounds, cptr\n");
  fprintf(fp,"         \n");
  fprintf(fp,"         nlat = (nptrs - 2*ngfcn) / sizelatcb\n");
  fprintf(fp,"         if( option .eq. 0 ) then\n");
  fprintf(fp,"            latinfo = nlat\n");
  fprintf(fp,"            return\n");
  fprintf(fp,"         end if\n");
  fprintf(fp,"\n");
  fprintf(fp,"         if( 1 .le. ilat  .and.  ilat .le. nlat ) then\n");
  fprintf(fp,"            base    = 2*ngfcn + (ilat - 1)*sizelatcb + 1\n");
  fprintf(fp,"            latid   = ptrs(base)\n");
  fprintf(fp,"            cs      = ptrs(base+1)\n");
  fprintf(fp,"            rank    = ptrs(base+2)\n");
  fprintf(fp,"            shape   = base + 3\n");
  fprintf(fp,"            bounds  = shape + maxrank\n");
  fprintf(fp,"            cptr    = bounds + 2*maxrank\n");
  fprintf(fp,"            if(      option .eq. 1 ) then\n");
  fprintf(fp,"               latinfo = latid\n");
  fprintf(fp,"            else if( option .eq. 2 ) then\n");
  fprintf(fp,"               latinfo = rank\n");
  fprintf(fp,"            else if( option .eq. 3 ) then\n");
  fprintf(fp,"               latinfo = shape\n");
  fprintf(fp,"            else if( option .eq. 4 ) then\n");
  fprintf(fp,"               if( 1 .le. j  .and.  j .le. rank ) then\n");
  fprintf(fp,"                  latinfo = ptrs(shape + j - 1)\n");
  fprintf(fp,"               else\n");
  fprintf(fp,"                  write(0,1000) j, rank, ilat\n");
  fprintf(fp,"1000              format(' latinfo: j = ',i2,' out of bounds for ',\n");
  fprintf(fp,"     &                   'rank ',i2,' lattice ',i2)\n");
  fprintf(fp,"                  latinfo = -1\n");
  fprintf(fp,"               end if\n");
  fprintf(fp,"            else if( option .eq. 5 ) then\n");
  fprintf(fp,"               if( 1 .le. j  .and.  j .le. rank ) then\n");
  fprintf(fp,"                  latinfo = ptrs(cptr + j-1)\n");
  fprintf(fp,"               else\n");
  fprintf(fp,"                  write(0,1000) j, rank, ilat\n");
  fprintf(fp,"                  latinfo = -1\n");
  fprintf(fp,"               end if\n");
  fprintf(fp,"            else if( option .eq. 6 ) then\n");
  fprintf(fp,"                  latinfo = l_ivprod(ptrs(shape),rank)\n");
  fprintf(fp,"            else if( option .eq. 7 ) then\n");
  fprintf(fp,"                  latinfo = bounds\n");
  fprintf(fp,"            else if( option .eq. 8 ) then\n");
  fprintf(fp,"               if( 1 .le. j  .and.  j .le. 2*rank ) then\n");
  fprintf(fp,"                  latinfo = ptrs(bounds + j - 1)\n");
  fprintf(fp,"               else\n");
  fprintf(fp,"                  write(0,1000) j, rank, ilat\n");
  fprintf(fp,"                  latinfo = -1\n");
  fprintf(fp,"               end if\n");
  fprintf(fp,"            else if( option .eq. 9 ) then\n");
  fprintf(fp,"                  latinfo = cs\n");
  fprintf(fp,"            end if\n");
  fprintf(fp,"         else\n");
  fprintf(fp,"            write(0,1100) ilat, nlat\n");
  fprintf(fp,"1100        format(' latinfo: ilat = ',i2,' out of bounds 1 - ',i2)\n");
  fprintf(fp,"            latinfo = -1\n");
  fprintf(fp,"         end if\n");
  fprintf(fp,"\n");
  fprintf(fp,"         return\n");
  fprintf(fp,"\n");
  fprintf(fp,"      end\n");
  fprintf(fp,"\n");
}

void make_c_init_main(FILE *fp, char *iname)
{
  int fn,i,j;
  offset_type *o;
  work_list *wk;

  fprintf(fp,"main(int argc, char **argv)\n");
  fprintf(fp,"{\n");
  fprintf(fp,"  char param_file[60];\n");
  fprintf(fp,"  FILE *fp;\n");
  fprintf(fp,"  double t;\n");
  fprintf(fp,"  double *lc;\n");
  fprintf(fp,"  int i;\n");
  fprintf(fp,"  extern char *optarg;\n");
  fprintf(fp,"  extern int optind, opterr,optopt;\n");
  fprintf(fp,"  int opt,argerr=0;\n");
  fprintf(fp,"  int rnpldone=0;\n");
  declare_coord_difs(fp); 
  declare_parameters(fp);
  declare_grids(fp); 
  declare_gfuncs(fp);
  declare_attributes(fp); 
  for(i=0;i<niniters;i++){
    for(wk=initers[i].work_refs;wk!=NULL;wk=wk->next){
      if(wk->work->ato){
        fprintf(fp,"  double *work%d;\n",wk->work->num);
        fprintf(fp,"  int   nwork%d;\n",wk->work->num);
      }
    }
  }
  fprintf(fp,"\n");
  fprintf(fp,"  /* initialize parameters */\n");
  for(i=0;i<nparams;i++){
    if(params[i].con!=2){
       fprintf(fp,"  set_%s=0;\n",params[i].name->n);
      if(vsize(params[i].size)==1){
        if(params[i].def){
          fprintf(fp,"  %s=",params[i].name->n);
          switch(params[i].type){
            case INT   : fprintf(fp,"%d;\n",params[i].def_val.i_ar[0]);
                         break;
            case FLOAT : fprintf(fp,"%.16g;\n",params[i].def_val.f_ar[0]);
                         break;
            case STRING: fprintf(fp,"cvec_alloc(%d);\n",strlen(params[i].def_val.s_ar[0])+1);
                         fprintf(fp,"  strcpy(%s,\"%s\");\n",params[i].name->n,params[i].def_val.s_ar[0]);
                         break;
          }
        }else if(params[i].type==STRING){
          fprintf(fp,"  %s=NULL;\n",params[i].name->n);
        }
      }else{
        switch(params[i].type){
          case IVEC   :
          case INT   : fprintf(fp,"  %s=ivec_alloc_n(%d,\"%s\");\n",params[i].name->n,
                               vsize(params[i].size),params[i].name->n);
                       break;
          case FLOAT : fprintf(fp,"  %s=vec_alloc_n(%d,\"%s\");\n",params[i].name->n,
                               vsize(params[i].size),params[i].name->n);
                       break;
          case STRING: fprintf(fp,"  %s=(char **)malloc(%d*sizeof(char *));\n",params[i].name->n,
                               vsize(params[i].size));
                       break;
        }
        if(params[i].def){
          for(j=0;j<vsize(params[i].size);j++){
            fprintf(fp,"  %s[%d]=",params[i].name->n,j);
            switch(params[i].type){
              case IVEC  :
              case INT   : fprintf(fp,"%d;\n",params[i].def_val.i_ar[j]);
                           break;
              case FLOAT : fprintf(fp,"%.16g;\n",params[i].def_val.f_ar[j]);
                           break;
              case STRING: fprintf(fp,"(char *)malloc(%d);\n",strlen(params[i].def_val.s_ar[j]));
                           fprintf(fp,"strcpy(%s[%d],\"%s\");\n",params[i].name->n,j,
                                   params[i].def_val.s_ar[j]);
                           break;
            }
          }
        }
      }
    }
  }

  for(i=0;i<nattribs;i++){
     fprintf(fp,"  set_%s=0;\n",attribs[i].name->n);
    switch(attribs[i].type){
      case INT   : fprintf(fp,"  %s=ivec_alloc_n(%d,\"%s\");\n",attribs[i].name->n,
                           attribs[i].size,attribs[i].name->n);
                   break;
      case FLOAT : fprintf(fp,"  %s=vec_alloc_n(%d,\"%s\");\n",attribs[i].name->n,
                           attribs[i].size,attribs[i].name->n);
                   break;
      case STRING: fprintf(fp,"  %s=(char **)malloc(%d*sizeof(char *));\n",attribs[i].name->n,
                           attribs[i].size);
                   break;
    }
    if(attribs[i].def){
      for(j=0;j<attribs[i].size;j++){
        fprintf(fp,"  %s[%d]=",attribs[i].name->n,j);
        switch(attribs[i].type){
          case INT   : fprintf(fp,"%d;\n",attribs[i].def_val.i_ar[j]);
                       break;
          case FLOAT : fprintf(fp,"%.16g;\n",attribs[i].def_val.f_ar[j]);
                       break;
          case STRING: fprintf(fp,"(char *)malloc(%d);\n",strlen(attribs[i].def_val.s_ar[j]));
                       fprintf(fp,"strcpy(%s[%d],\"%s\");\n",attribs[i].name->n,j,
                               attribs[i].def_val.s_ar[j]);
                       break;
        }
      }
    }
  }
  fprintf(fp,"\n\n");
  fprintf(fp,"  /* check command line parameters */\n");
  fprintf(fp,"  while((opt=getopt(argc,argv,\"p:\"))!=EOF){\n");
  fprintf(fp,"    switch(opt){\n");
  fprintf(fp,"      case 'p' :\n");
  fprintf(fp,"        sread_parameters(optarg");
  params_call(fp);
  fprintf(fp,");\n");
  fprintf(fp,"        break;\n");
  fprintf(fp,"      case '?' :\n");
  fprintf(fp,"        argerr=1;\n");
  fprintf(fp,"        break;\n");
  fprintf(fp,"    }\n");
  fprintf(fp,"  }\n");
  fprintf(fp,"  if(argerr){\n");
  fprintf(fp,"    fprintf(stderr,\" %s\\n\");\n",iname);
  fprintf(fp,"    fprintf(stderr,\"Usage: %s\\n\");\n",iname);
  fprintf(fp,"    fprintf(stderr,\"     [ -p \\\"parameter:=value\\\" ]\\n\");\n");
  fprintf(fp,"    fprintf(stderr,\"     [ parameter_file ]\\n\");\n");
  fprintf(fp,"    exit(0);\n");
  fprintf(fp,"  }\n");
  fprintf(fp,"  if(optind<argc){\n");
  fprintf(fp,"     strcpy(param_file,argv[optind]);\n");
  fprintf(fp,"  }else{\n");
  fprintf(fp,"     printf(\"%s: Enter name of parameter file: \");\n",iname);
  fprintf(fp,"     fflush(stdout);\n");
  fprintf(fp,"     scanf(\"%%s\",param_file);\n");
  fprintf(fp,"  }\n");
  fprintf(fp,"  if((fp=fopen(param_file,\"r\"))==NULL){\n");
  fprintf(fp,"    fprintf(stderr,\"%s: Unable to open parameter file %%s\\n\",param_file);\n",
          iname);
  fprintf(fp,"    exit(0);\n");
  fprintf(fp,"  }\n");
  fprintf(fp,"  fclose(fp);\n");
  fprintf(fp,"\n");
  fprintf(fp,"  read_parameters(param_file");
  params_call(fp);
  fprintf(fp,");\n");
  fprintf(fp,"  read_attributes(param_file");
  attribs_call(fp);
  fprintf(fp,");\n");
  fprintf(fp,"  read_attributes(\".rnpl.attributes\"");
  attribs_call(fp);
  fprintf(fp,");\n");
  fprintf(fp,"  /* initialize coordinate differentials */\n");
  init_coord_difs(fp);
  fprintf(fp,"  /* initialize grids */\n");
  init_grids(fp);
  fprintf(fp,"  /* initialize grid functions */\n");
  init_gfuncs(fp);
  fprintf(fp,"  if(!check_params_attribs(");
  check_params_call(fp);
  fprintf(fp,")){\n");
  fprintf(fp,"    fprintf(stderr,\"%s: Unable to continue.\\n\");\n",iname);
  fprintf(fp,"    exit(0);\n");
  fprintf(fp,"  }\n");
  fprintf(fp,"\n");
  fprintf(fp,"  iter*=(int)pow(2.0,(double)level);\n");
  fprintf(fp,"\n");
  fprintf(fp,"  t=start_t;\n");
  fprintf(fp,"  i=s_step;\n");
  fprintf(fp,"  /* fix a default */\n");
  fprintf(fp,"  if(!set_epsiterid)\n");
  fprintf(fp,"    epsiterid=epsiter;\n");
  for(i=0;i<niniters;i++){
    for(wk=initers[i].work_refs;wk!=NULL;wk=wk->next){
      if(wk->work->ato){
        fprintf(fp,"  nwork%d=",wk->work->num);
        ex_to_string(fp,wk->work->expr);
        fprintf(fp,";\n");
        fprintf(fp,"  work%d=vec_alloc_n(nwork%d,\"work%d\");\n",wk->work->num,
        wk->work->num,wk->work->num);
      }
    }
    fprintf(fp,"  ");
    init_call(fp,&initers[i]);
    for(wk=initers[i].work_refs;wk!=NULL;wk=wk->next){
      if(wk->work->ato){
        fprintf(fp,"  free(work%d);\n",wk->work->num);
      }
    }
  }
   if(neediditer){
     fprintf(fp,"  iter_gfuncs(&rnpldone,maxstepid,epsiterid,");
     update_iter_call_args(fp);
     fprintf(fp,");\n");
  /* swap time levels */
     fprintf(fp,"  ");
     swap_top_call(fp);
  }
  fprintf(fp,"  ");
  dump_state_call(fp);
  cleanup(fp);
  fprintf(fp,"  gft_close_all();\n");
  fprintf(fp,"}\n\n");
}

void make_f77_init_main(FILE *fp,char *iname)
{
  int i;
  work_list *wk;
  fprintf(fp,"!-----------------------------------------------------------------------\n");
  fprintf(fp,"!\n");
  fprintf(fp,"!   This is the f77 init main\n");
  fprintf(fp,"!    \n");
  fprintf(fp,"!-----------------------------------------------------------------------\n");
  fprintf(fp,"\n");
  fprintf(fp,"      program        initer\n");
  fprintf(fp,"\n");
  fprintf(fp,"      implicit       none\n");
  fprintf(fp,"\n");
  fprintf(fp,"      include       'sys_param.inc'\n");
  fprintf(fp,"      include       'gfuni0.inc'\n");
  fprintf(fp,"      include       'globals.inc'\n");
  fprintf(fp,"      include       'other_glbs.inc'\n");
  fprintf(fp,"\n");
  fprintf(fp,"      integer      iter_gfuncs,   rc_iter_gfuncs\n");
  fprintf(fp,"      integer      iargc, getu, indlnb\n");
  fprintf(fp,"      integer      mmaloc, mmdeal\n");
  fprintf(fp,"      integer      argc\n");
  fprintf(fp,"      character*64 argv(64)\n");
  fprintf(fp,"\n");
  fprintf(fp,"      character*64 param_file,optarg\n");
  fprintf(fp,"      integer      fp,i,steps,rc,argerr,got_pf,ret\n");
  fprintf(fp,"      real*8       t\n");
  fprintf(fp,"      integer      update_gfuncs,check_params_attribs\n");
  for(i=0;i<niniters;i++){
    for(wk=initers[i].work_refs;wk!=NULL;wk=wk->next){
      if(wk->work->ato){
        fprintf(fp,"      integer work%d, nwork%d\n",wk->work->num,
                wk->work->num);
      }
    }
  }
  fprintf(fp,"      \n");
  fprintf(fp,"      argc=iargc()\n");
  fprintf(fp,"      do i=1,argc\n");
  fprintf(fp,"        call getarg(i,argv(i))\n");
  fprintf(fp,"      end do\n");
  fprintf(fp,"      call mmini(memsiz)\n");
  fprintf(fp,"      call init_params_attribs()\n");
  fprintf(fp,"      i=1\n");
  fprintf(fp,"      argerr=0\n");
  fprintf(fp,"      got_pf=0\n");
  fprintf(fp,"100   continue\n");
  fprintf(fp,"      if(i.le.argc) then\n");
  fprintf(fp,"        optarg=argv(i)\n");
  fprintf(fp,"        if(optarg .eq. '-p') then\n");
  fprintf(fp,"          i=i+1\n");
  fprintf(fp,"          optarg=argv(i)\n");
  fprintf(fp,"          call sread_parameters(optarg)\n");
  fprintf(fp,"        else if(optarg(1:1).eq.'-') then\n");
  fprintf(fp,"          argerr=1\n");
  fprintf(fp,"        else\n");
  fprintf(fp,"          param_file=optarg\n");
  fprintf(fp,"          got_pf=1\n");
  fprintf(fp,"        end if\n");
  fprintf(fp,"        i=i+1\n");
  fprintf(fp,"        goto 100\n");
  fprintf(fp,"      end if\n");
  fprintf(fp,"      if(argerr .eq. 1) then\n");
  fprintf(fp,"        write(*,*) 'Usage: %s'\n",iname);
  fprintf(fp,"        write(*,*) '     [ -p \"parameter:=value\" ]'\n");
  fprintf(fp,"        write(*,*) '     [ parameter_file ]'\n");
  fprintf(fp,"        stop\n");
  fprintf(fp,"      end if\n");
  fprintf(fp,"      if(got_pf .ne. 1) then\n");
  fprintf(fp,"101     continue \n");
  fprintf(fp,"        write(*,*) 'Enter name of parameter file: '\n");
  fprintf(fp,"        read(*,1010,end=101,err=101) param_file\n");
  fprintf(fp,"1010    format(a)  \n");
  fprintf(fp,"      end if\n");
  fprintf(fp,"      fp=getu()\n");
  fprintf(fp,"      open(unit=fp,file=param_file(1:indlnb(param_file)),\n");
  fprintf(fp,"     &     status = 'old', form = 'unformatted', iostat = rc)\n");
  fprintf(fp,"      if(rc .ne. 0) then\n");
  fprintf(fp,"        write(*,*) 'Unable to open initial data file '//\n");
  fprintf(fp,"     &             param_file(1:indlnb(param_file))\n");
  fprintf(fp,"        stop \n");
  fprintf(fp,"      end if\n");
  fprintf(fp,"      close(fp)\n");
  fprintf(fp,"\n");
  fprintf(fp,"      call read_parameters(param_file(1:indlnb(param_file)))\n");
  fprintf(fp,"      call read_attributes(param_file(1:indlnb(param_file)))\n");
  fprintf(fp,"      call read_attributes('.rnpl.attributes')\n");
  fprintf(fp,"      ret=check_params_attribs()\n");
  fprintf(fp,"      if(ret.eq.0) then\n");
  fprintf(fp,"        write(*,*) '%s: Unable to continue.'\n",iname);
  fprintf(fp,"        stop\n");
  fprintf(fp,"      end if\n");
  fprintf(fp,"      call init_coord_difs()\n");
  fprintf(fp,"      call init_lats()\n");
  fprintf(fp,"      call init_gfuncs()\n");
  fprintf(fp,"\n");
  fprintf(fp,"      rmod = rmod * 2.0**level\n");
  fprintf(fp,"      iter = iter * 2.0**level\n");
  fprintf(fp,"\n");
  fprintf(fp,"      t = start_t\n");
  fprintf(fp,"      i = s_step\n");
  fprintf(fp,"      if(set_epsiterid.eq.0) then\n");
  fprintf(fp,"        epsiterid=epsiter\n");
  fprintf(fp,"      end if\n");
  for(i=0;i<niniters;i++){
    for(wk=initers[i].work_refs;wk!=NULL;wk=wk->next){
      if(wk->work->ato){
        sprintf(forbuf,"      nwork%d=",wk->work->num);
        fort_out(fp,forbuf);
        ex_to_string_f(fp,wk->work->expr);
        fort_out(fp,"\n");
        sprintf(forbuf,"      work%d=mmaloc(nwork%d)\n",wk->work->num,
                wk->work->num);
        fort_out(fp,forbuf);
        sprintf(forbuf,"      if(work%d .eq. -1) then\n",wk->work->num);
        fort_out(fp,forbuf);
        fort_out(fp,"        write(*,*)'ERROR: unable to allocate work array'\n");
        fort_out(fp,"        write(*,*)'increase memsiz and recompile'\n");
        fort_out(fp,"        stop\n");
        fort_out(fp,"      end if\n");
      }
    }
    fort_out(fp,"      ");
    init_call(fp,&initers[i]);
    for(wk=initers[i].work_refs;wk!=NULL;wk=wk->next){
      if(wk->work->ato){
        sprintf(forbuf,"      work%d=mmdeal(work%d,nwork%d)\n",wk->work->num,
                wk->work->num,wk->work->num);
        fort_out(fp,forbuf);
      }
    }
  }
   if(neediditer)
     fprintf(fp,"      rc_iter_gfuncs = iter_gfuncs(t)\n");
  fprintf(fp,"      call dump_state(t,i,in_file)\n");
  fprintf(fp,"      call cleanup()\n");
  fprintf(fp,"      stop\n");
  fprintf(fp,"\n");
  fprintf(fp,"      end\n");
  fprintf(fp,"\n");
  fprintf(fp,"      \n");
}

void make_par_c_main(FILE *fp, char *oname)
{
  fprintf(fp,"int main(int argc, char **argv)\n");
  fprintf(fp,"{\n");
  fprintf(fp,"  extern char *optarg;\n");
  fprintf(fp,"  extern int optind,opterr,optopt;\n");
  fprintf(fp,"  char **myargv;\n");
  fprintf(fp,"  int myargc,opt,i,j,argerr=0;\n");
  fprintf(fp,"  int argptr,parg=0;\n");
  fprintf(fp,"  char *flag,*param,*pfilename;\n");
  fprintf(fp,"  char parname[60],dirname[256],syscom[256];\n");
  fprintf(fp,"  double *parpar,pval;\n");
  fprintf(fp,"  int mpisize,myproc,num_groups,psize,left_over;\n");
  fprintf(fp,"  \n");
  fprintf(fp,"   MPI_Init(&argc,&argv);\n");
  fprintf(fp,"  pfilename=cvec_alloc_n(100,\"parameter file name\");\n");
  fprintf(fp,"  myargv=(char **)malloc(sizeof(char *)*(argc+1));\n");
  fprintf(fp,"  if(myargv==NULL){\n");
  fprintf(fp,"     fprintf(stderr,\"%s: Can't allocate memory for %%d char *'s\\n\",argc);\n",oname);
  fprintf(fp,"     exit(0);\n");
  fprintf(fp,"  }\n");
  fprintf(fp,"  for(i=0,j=0;j<argc;){\n");
  fprintf(fp,"     if(!strcmp(\"-v\",argv[j])){\n");
  fprintf(fp,"        argptr=j;\n");
  fprintf(fp,"        i+=2;\n");
  fprintf(fp,"        j+=2;\n");
  fprintf(fp,"     }else{\n");
  fprintf(fp,"        myargv[i++]=argv[j++];\n");
  fprintf(fp,"     }\n");
  fprintf(fp,"  }\n");
  fprintf(fp,"  myargc=i;\n");
  fprintf(fp,"  while((opt=getopt(argc,argv,\"p:v:\"))!=EOF){\n");
  fprintf(fp,"    switch(opt){\n");
  fprintf(fp,"      case 'p' :\n");
  fprintf(fp,"         break; /* do nothing */\n");
  fprintf(fp,"      case 'v' :\n");
  fprintf(fp,"         if(parg){\n");
  fprintf(fp,"            argerr=1;\n");
  fprintf(fp,"            fprintf(stderr,\"%s: only one parallel parameter supported.\\n\");\n",oname);
  fprintf(fp,"         }else{\n");
  fprintf(fp,"            parg=1;\n");
  fprintf(fp,"            parpar=vec_alloc_n(1+FVEL,\"parallel parameter\");\n");
  fprintf(fp,"            for(i=0;isspace(optarg[i]);i++);\n");
  fprintf(fp,"            for(j=0;optarg[i] && !isspace(optarg[i]) && !(optarg[i]==':' || optarg[i]=='=');\n");
  fprintf(fp,"                parname[j++]=optarg[i++]);\n");
  fprintf(fp,"          parname[j]=0;\n");
  fprintf(fp,"            if(sget_param(optarg,parname,\"fvec\",1+FVEL,&parpar,1)!=1){\n");
  fprintf(fp,"            fprintf(stderr,\"%s: error reading parameter <%%s>\\n\",optarg);\n",oname);
  fprintf(fp,"            exit(0);\n");
  fprintf(fp,"          }\n");
  fprintf(fp,"         }\n");
  fprintf(fp,"         break;\n");
  fprintf(fp,"      case '?' :\n");
  fprintf(fp,"         argerr=1;\n");
  fprintf(fp,"         break;\n");
  fprintf(fp,"    }\n");
  fprintf(fp,"  }\n");
  fprintf(fp,"  if(argerr){\n");
  fprintf(fp,"     fprintf(stderr,\"Usage:\\n\");\n");
  fprintf(fp,"     fprintf(stderr,\"  %s [ -p \\\"parameter:=value\\\" ]\\n\");\n",oname);
  fprintf(fp,"     fprintf(stderr,\"     [ -v \\\"parallel_parameter:=fvec\\\" ]\\n\");\n");
  fprintf(fp,"     fprintf(stderr,\"     [ parameter_file ]\\n\");\n");
  fprintf(fp,"       exit(0);\n");
  fprintf(fp,"  }\n");
  fprintf(fp,"  if(optind<argc){\n");
  fprintf(fp,"     strcpy(pfilename,argv[optind]);\n");
  fprintf(fp,"  }else{\n");
  fprintf(fp,"     printf(\"Enter name of parameter file: \");\n");
  fprintf(fp,"     fflush(stdout);\n");
  fprintf(fp,"     scanf(\"%%s\",pfilename);\n");
  fprintf(fp,"     myargv[myargc++]=pfilename;\n");
  fprintf(fp,"  }\n");
  fprintf(fp,"  if(parg){\n");
  fprintf(fp,"      MPI_Comm_size(MPI_COMM_WORLD,&mpisize);\n");
  fprintf(fp,"      MPI_Comm_rank(MPI_COMM_WORLD,&myproc);\n");
  fprintf(fp,"    psize=size_fvec(parpar);\n");
  fprintf(fp,"     num_groups=psize/mpisize;\n");
  fprintf(fp,"    left_over=psize%%mpisize;\n");
  fprintf(fp,"     flag=cvec_alloc_n(3,\"flag\");\n");
  fprintf(fp,"     param=cvec_alloc_n(256,\"param\");\n");
  fprintf(fp,"     for(i=0;i<num_groups;i++){\n");
  fprintf(fp,"        pval=get_fvec(parpar,i*mpisize+myproc);\n");
  fprintf(fp,"        sprintf(dirname,\"%s_%%s=%%g\",parname,pval);\n",oname);
  fprintf(fp,"        mkdir(dirname,0777);\n");
  fprintf(fp,"        sprintf(syscom,\"cp .rnpl.attributes %%s %%s\",pfilename,dirname);\n");
  fprintf(fp,"        system(syscom);\n");
  fprintf(fp,"        chdir(dirname);\n");
  fprintf(fp,"        sprintf(flag,\"-p\");\n");
  fprintf(fp,"        sprintf(param,\"%%s:=%%g\",parname,pval);\n");
  fprintf(fp,"        myargv[argptr]=flag;\n");
  fprintf(fp,"        myargv[argptr+1]=param;\n");
  fprintf(fp,"        sub_main(myargc,myargv);\n");
  fprintf(fp,"        chdir(\"..\");\n");
  fprintf(fp,"     }\n");
  fprintf(fp,"    if(myproc<left_over){\n");
  fprintf(fp,"        pval=get_fvec(parpar,i*mpisize+myproc);\n");
  fprintf(fp,"        sprintf(dirname,\"%s_%%s=%%g\",parname,pval);\n",oname);
  fprintf(fp,"        mkdir(dirname,0777);\n");
  fprintf(fp,"        sprintf(syscom,\"cp .rnpl.attributes %%s %%s\",pfilename,dirname);\n");
  fprintf(fp,"        system(syscom);\n");
  fprintf(fp,"        chdir(dirname);\n");
  fprintf(fp,"        sprintf(flag,\"-p\");\n");
  fprintf(fp,"        sprintf(param,\"%%s:=%%g\",parname,pval);\n");
  fprintf(fp,"        myargv[argptr]=flag;\n");
  fprintf(fp,"        myargv[argptr+1]=param;\n");
  fprintf(fp,"        sub_main(myargc,myargv);\n");
  fprintf(fp,"        chdir(\"..\");\n");
  fprintf(fp,"     }\n");
  fprintf(fp,"  }else{\n");
  fprintf(fp,"     sub_main(myargc,myargv);\n");
  fprintf(fp,"  }\n");
  fprintf(fp,"  MPI_Finalize();\n");
  fprintf(fp,"}\n");
}

void make_c_main(FILE *fp, char *oname)
{
  int fn,i,j;
  offset_type *o;
  work_list *wl;

   fprintf(fp,"int got_signal;\n");
  fprintf(fp,"void sig_handler(int sig)\n");
  fprintf(fp,"{\n");
  fprintf(fp,"  got_signal=sig;\n");
  fprintf(fp,"}\n");
  fprintf(fp,"\n");
  fprintf(fp,"#define IVEL 4\n");
  fprintf(fp,"#define FVEL 4\n");
  fprintf(fp,"\n");
  if(parallel==TRIV){
     fprintf(fp,"void sub_main(int argc, char **argv);\n\n");
     make_par_c_main(fp,oname);
     fprintf(fp,"\nvoid sub_main(int argc, char **argv)\n");
  }else if(parallel==0){
     fprintf(fp,"int main(int argc, char **argv)\n");
  }
  fprintf(fp,"{\n");
  fprintf(fp,"  FILE *fp;\n");
  fprintf(fp,"  char param_file[60];\n");
  fprintf(fp,"  int i,steps,ot;\n");
  fprintf(fp,"  double %s;\n",coords[0].c_names->name->n);
  fprintf(fp,"  double *lc;\n");
  fprintf(fp,"  time_t st,ed;\n");
  fprintf(fp,"  extern char *optarg;\n");
  fprintf(fp,"  extern int optind, opterr,optopt;\n");
  fprintf(fp,"  int opt,argerr=0;\n");
  fprintf(fp,"  int rnpldone=0;\n");
  declare_coord_difs(fp); 
  declare_parameters(fp);
  declare_grids(fp); 
  declare_gfuncs(fp);
  declare_attributes(fp); 
  fprintf(fp,"\n");
  fprintf(fp,"  optarg=NULL; optind=1; opterr=1; optopt=0;\n");
  fprintf(fp,"  /* initialize parameters */\n");
  for(i=0;i<nparams;i++){
    if(params[i].con!=2){
       fprintf(fp,"  set_%s=0;\n",params[i].name->n);
      if(vsize(params[i].size)==1){
        if(params[i].def){
          fprintf(fp,"  %s=",params[i].name->n);
          switch(params[i].type){
            case INT   : fprintf(fp,"%d;\n",params[i].def_val.i_ar[0]);
                         break;
            case FLOAT : fprintf(fp,"%.16g;\n",params[i].def_val.f_ar[0]);
                         break;
            case STRING: fprintf(fp,"cvec_alloc(%d);\n",strlen(params[i].def_val.s_ar[0])+1);
                         fprintf(fp,"  strcpy(%s,\"%s\");\n",params[i].name->n,params[i].def_val.s_ar[0]);
                         break;
          }
        }else if(params[i].type==STRING){
          fprintf(fp,"  %s=NULL;\n",params[i].name->n);
        }
      }else{
        switch(params[i].type){
          case IVEC   :
          case INT   : fprintf(fp,"  %s=ivec_alloc_n(%d,\"%s\");\n",params[i].name->n,
                               vsize(params[i].size),params[i].name->n);
                       break;
          case FLOAT : fprintf(fp,"  %s=vec_alloc_n(%d,\"%s\");\n",params[i].name->n,
                               vsize(params[i].size),params[i].name->n);
                       break;
          case STRING: fprintf(fp,"  %s=(char **)malloc(%d*sizeof(char *));\n",params[i].name->n,
                               vsize(params[i].size));
                       break;
        }
        if(params[i].def){
          for(j=0;j<vsize(params[i].size);j++){
            fprintf(fp,"  %s[%d]=",params[i].name->n,j);
            switch(params[i].type){
              case IVEC  :
              case INT   : fprintf(fp,"%d;\n",params[i].def_val.i_ar[j]);
                           break;
              case FLOAT : fprintf(fp,"%.16g;\n",params[i].def_val.f_ar[j]);
                           break;
              case STRING: fprintf(fp,"(char *)malloc(%d);\n",strlen(params[i].def_val.s_ar[j]));
                           fprintf(fp,"strcpy(%s[%d],\"%s\");\n",params[i].name->n,j,
                                   params[i].def_val.s_ar[j]);
                           break;
            }
          }
        }
      }
    }
  }

  for(i=0;i<nattribs;i++){
     fprintf(fp,"  set_%s=0;\n",attribs[i].name->n);
    switch(attribs[i].type){
      case INT   : fprintf(fp,"  %s=ivec_alloc_n(%d,\"%s\");\n",attribs[i].name->n,
                           attribs[i].size,attribs[i].name->n);
                   break;
      case FLOAT : fprintf(fp,"  %s=vec_alloc_n(%d,\"%s\");\n",attribs[i].name->n,
                           attribs[i].size,attribs[i].name->n);
                   break;
      case STRING: fprintf(fp,"  %s=(char **)malloc(%d*sizeof(char *));\n",attribs[i].name->n,
                           attribs[i].size);
                   break;
    }
    if(attribs[i].def){
      for(j=0;j<attribs[i].size;j++){
        fprintf(fp,"  %s[%d]=",attribs[i].name->n,j);
        switch(attribs[i].type){
          case INT   : fprintf(fp,"%d;\n",attribs[i].def_val.i_ar[j]);
                       break;
          case FLOAT : fprintf(fp,"%.16g;\n",attribs[i].def_val.f_ar[j]);
                       break;
          case STRING: fprintf(fp,"(char *)malloc(%d);\n",strlen(attribs[i].def_val.s_ar[j]));
                       fprintf(fp,"strcpy(%s[%d],\"%s\");\n",attribs[i].name->n,j,
                               attribs[i].def_val.s_ar[j]);
                       break;
        }
      }
    }
  }
  fprintf(fp,"\n\n");

  fprintf(fp,"  /* check command line parameters */\n");
  fprintf(fp,"  while((opt=getopt(argc,argv,\"p:\"))!=EOF){\n");
  fprintf(fp,"    switch(opt){\n");
  fprintf(fp,"      case 'p' :\n");
  fprintf(fp,"        sread_parameters(optarg");
  params_call(fp);
  fprintf(fp,");\n");
  fprintf(fp,"        break;\n");
  fprintf(fp,"      case '?' :\n");
  fprintf(fp,"        argerr=1;\n");
  fprintf(fp,"        break;\n");
  fprintf(fp,"    }\n");
  fprintf(fp,"  }\n");
  fprintf(fp,"  if(argerr){\n");
  fprintf(fp,"    fprintf(stderr,\" %s\\n\");\n",oname);
  fprintf(fp,"    fprintf(stderr,\"Usage: %s\\n\");\n",oname);
  fprintf(fp,"    fprintf(stderr,\"     [ -p \\\"parameter:=value\\\" ]\\n\");\n");
  fprintf(fp,"    fprintf(stderr,\"     [ parameter_file ]\\n\");\n");
  fprintf(fp,"    exit(0);\n");
  fprintf(fp,"  }\n");
  fprintf(fp,"  if(optind<argc){\n");
  fprintf(fp,"     strcpy(param_file,argv[optind]);\n");
  fprintf(fp,"  }else{\n");
  fprintf(fp,"     printf(\"%s: Enter name of parameter file: \");\n",oname);
  fprintf(fp,"     fflush(stdout);\n");
  fprintf(fp,"     scanf(\"%%s\",param_file);\n");
  fprintf(fp,"  }\n");
  fprintf(fp,"  if((fp=fopen(param_file,\"r\"))==NULL){\n");
  fprintf(fp,"    fprintf(stderr,\"%s: Unable to open parameter file %%s\\n\",param_file);\n",
          oname);
  fprintf(fp,"    exit(0);\n");
  fprintf(fp,"  }\n");
  fprintf(fp,"  fclose(fp);\n");
  fprintf(fp,"\n");
  fprintf(fp,"  read_parameters(param_file");
  params_call(fp);
  fprintf(fp,");\n");
  fprintf(fp,"  read_attributes(param_file");
  attribs_call(fp);
  fprintf(fp,");\n");
  fprintf(fp,"  read_attributes(\".rnpl.attributes\"");
  attribs_call(fp);
  fprintf(fp,");\n");
  fprintf(fp,"  /* initialize coordinate differentials */\n");
  init_coord_difs(fp);
  fprintf(fp,"  /* initialize grids */\n");
  init_grids(fp);
  fprintf(fp,"  /* initialize grid functions */\n");
  init_gfuncs(fp);
  fprintf(fp,"  ");
  read_state_call(fp);
  fprintf(fp,"  if(!check_params_attribs(");
  check_params_call(fp);
  fprintf(fp,")){\n");
  fprintf(fp,"    fprintf(stderr,\"%s: Unable to continue.\\n\");\n",oname);
  fprintf(fp,"    exit(0);\n");
  fprintf(fp,"  }\n");
  fprintf(fp,"\n");
  fprintf(fp,"  iter*=(int)pow(2.0,(double)level);\n");
  fprintf(fp,"\n");
  fprintf(fp,"  /* set up signal handler */\n");
  fprintf(fp,"  got_signal=FALSE;\n");
  fprintf(fp,"  signal(SIGQUIT,sig_handler);\n");
  fprintf(fp,"  signal(SIGINT,sig_handler);\n");
  fprintf(fp,"  st=time(&st);\n");
  fprintf(fp,"  rmod=-1;\n");
  fprintf(fp,"  %s=start_t;\n",coords[0].c_names->name->n);
  fprintf(fp,"  i=s_step;\n");
  fprintf(fp,"  rnpldone=0;\n");
  fprintf(fp,"  /* fix a couple of defaults */\n");
  fprintf(fp,"  if(!set_epsiterid)\n");
  fprintf(fp,"    epsiterid=epsiter;\n");
  fprintf(fp,"  if(!set_trace){\n");
  fprintf(fp,"    if(trace[0]<output[0]){\n");
  fprintf(fp,"      free(trace);\n");
  fprintf(fp,"      trace=ivec_alloc(output[0]*IVEL+1);\n");
  fprintf(fp,"    }\n");
  fprintf(fp,"    rivcpy(trace,output,output[0]*IVEL+1);\n");
  fprintf(fp,"  }\n");
  fprintf(fp,"  /* fix ivec indicies */\n");
  fprintf(fp,"  fixup_ivec(0,iter,level,output);\n");
  fprintf(fp,"  fixup_ivec(0,iter,level,trace);\n\n");
  fprintf(fp,"  if(i==0){\n");
  fprintf(fp,"    if(do_ivec(i,iter,trace))\n");
  fprintf(fp,"      fprintf(stdout,\"Starting evolution. step: %%d at t=%%g\\n\",i,%s);\n",
          coords[0].c_names->name->n);
  fprintf(fp,"    if(((rmod!=-1) && (i%%rmod==0)) || do_ivec(i,iter,output)){\n");
  fprintf(fp,"      ");
  output_call(fp);
  fprintf(fp,"    }\n");
  fprintf(fp,"  }\n");
  fprintf(fp,"  for(i++;i<=iter && !rnpldone;i++){\n");
  fprintf(fp,"    steps=update_gfuncs(&rnpldone,maxstep,epsiter,");
  update_iter_call_args(fp);
  fprintf(fp,");\n");
  fprintf(fp,"    ");
  swap_call(fp);
  fprintf(fp,"    %s+=%s;\n",coords[0].c_names->name->n,coord_difs[0][0]->n);
  fprintf(fp,"    if(do_ivec(i,iter,trace))\n");
  fprintf(fp,"      printf(\"step: %%d  t=%%g  steps=%%d\\n\",i,%s,steps);\n",
          coords[0].c_names->name->n);
  fprintf(fp,"    if(((rmod!=-1) && (i%%rmod==0)) || do_ivec(i,iter,output)){\n");
  fprintf(fp,"      ");
  output_call(fp);
  fprintf(fp,"    }\n");
  fprintf(fp,"    if(got_signal){\n");
  fprintf(fp,"      handler(got_signal,&rnpldone,&rmod,fname,out_gf);\n");
  fprintf(fp,"      got_signal=FALSE;\n");
  fprintf(fp,"      signal(SIGQUIT,sig_handler);\n");
  fprintf(fp,"      signal(SIGINT,sig_handler);\n");
  fprintf(fp,"    }\n");
  fprintf(fp,"  }\n");
  fprintf(fp,"  ed=time(&ed);\n");
  fprintf(fp,"  fprintf(stdout,\"Elapsed: %%lu sec\\n\",ed-st);\n");
  fprintf(fp,"  s_step=i-1;\n  ");
  dump_state_call(fp);
  cleanup(fp);
  fprintf(fp,"  gft_close_all();\n");
  fprintf(fp,"}\n\n");
}

void make_par_f77_main(FILE *fp, char *oname)
{
  fprintf(fp,"!-----------------------------------------------------------------------\n");
  fprintf(fp,"!\n");
  fprintf(fp,"!   This is the f77 main\n");
  fprintf(fp,"!    \n");
  fprintf(fp,"!-----------------------------------------------------------------------\n");
  fprintf(fp,"\n");
  fprintf(fp,"      program     main\n");
  fprintf(fp,"      implicit       none\n");
  fprintf(fp,"\n");
  fprintf(fp,"      include       'sys_param.inc'\n");
  fprintf(fp,"      include       'gfuni0.inc'\n");
   fprintf(fp,"      integer argc, myargc\n");
   fprintf(fp,"      character*64 argv(64),myargv(64)\n");
  fprintf(fp,"      include       'globals.inc'\n");
  fprintf(fp,"      include       'other_glbs.inc'\n");
  fprintf(fp,"      include       'mpif.h'\n");
  fprintf(fp,"\n");
  fprintf(fp,"      character*64 r8tostring\n");
  fprintf(fp,"      character*64 dirname,parname,syscom,flag,param,pvals\n");
  fprintf(fp,"      real*8       pval,get_fvec\n");
  fprintf(fp,"      integer      size_fvec,num_groups,left_over,mkdir,chdir\n");
  fprintf(fp,"      integer      sget_fvec_param,ierr,mpisize,myproc,psize\n");
  fprintf(fp,"      integer      iargc,getu,indlnb,do_out,got_pf\n");
  fprintf(fp,"      integer      argerr,parg,argptr,skblnk,k\n");
  fprintf(fp,"      integer      IVEL\n");
  fprintf(fp,"      parameter  ( IVEL = 4 )\n");
  fprintf(fp,"\n");
  fprintf(fp,"      character*64 pfilename,optarg\n");
  fprintf(fp,"      integer      fp,i,j,steps,rc,ot,max,ret\n");
  fprintf(fp,"      real*8       t,parpar(64)\n");
  fprintf(fp,"      integer      update_gfuncs, query_interrupt, check_params_attribs\n\n");
   fprintf(fp,"      call MPI_Init(rc)\n");
   fprintf(fp,"      argc=iargc()\n");
   fprintf(fp,"      do i=1,argc\n");
   fprintf(fp,"        call getarg(i,argv(i))\n");
   fprintf(fp,"      end do\n");
   fprintf(fp,"      i=1\n");
   fprintf(fp,"      j=1\n");
   fprintf(fp,"50    continue\n");
   fprintf(fp,"      if(j.le.argc) then\n");
   fprintf(fp,"        if('-v' .eq. argv(j)) then\n");
   fprintf(fp,"          argptr=j\n");
   fprintf(fp,"          i=i+2\n");
   fprintf(fp,"          j=j+2\n");
   fprintf(fp,"        else\n");
   fprintf(fp,"          myargv(i)=argv(j)\n");
   fprintf(fp,"          i=i+1\n");
   fprintf(fp,"          j=j+1\n");
   fprintf(fp,"        end if\n");
   fprintf(fp,"        goto 50\n");
   fprintf(fp,"      end if\n");
   fprintf(fp,"      myargc=i-1\n");
  fprintf(fp,"      i=1\n");
  fprintf(fp,"      argerr=0\n");
  fprintf(fp,"      got_pf=0\n");
  fprintf(fp,"      parg=0\n");
  fprintf(fp,"100   continue\n");
  fprintf(fp,"      if(i.le.argc) then\n");
  fprintf(fp,"        optarg=argv(i)\n");
  fprintf(fp,"        if(optarg .eq. '-p') then\n");
  fprintf(fp,"          i=i+1\n");
  fprintf(fp,"        else if(optarg .eq. '-v') then\n");
  fprintf(fp,"          i=i+1\n");
  fprintf(fp,"          optarg=argv(i)\n");
  fprintf(fp,"          if(parg.eq.1) then\n");
  fprintf(fp,"            argerr=1\n");
  sprintf(forbuf,"            write(*,*) '%s: ",oname);
  fort_out(fp,forbuf);
  fort_out(fp,"only one parallel parameter supported.'\n");
  fprintf(fp,"          else\n");
  fprintf(fp,"            parg=1\n");
  fprintf(fp,"            j=skblnk(optarg,1)\n");
  fprintf(fp,"            k=index(optarg,':')-1\n");
  fprintf(fp,"            if(k.le.0) then\n");
  fprintf(fp,"              k=index(optarg,'=')-1\n");
  fprintf(fp,"            end if\n");
  fprintf(fp,"            parname=optarg(j:k)\n");
  fprintf(fp,"            ret=sget_fvec_param(optarg,optarg(j:k),parpar,64)\n");
  fprintf(fp,"            if(ret.ne.1) then\n");
  sprintf(forbuf,"              write(*,*) '%s: ",oname);
  fort_out(fp,forbuf);
  fort_out(fp,"error reading param '");
  fort_out(fp,"//optarg\n");
  fprintf(fp,"              stop\n");
  fprintf(fp,"            end if\n");
  fprintf(fp,"          end if\n");
  fprintf(fp,"        else if(optarg(1:1).eq.'-') then\n");
  fprintf(fp,"          argerr=1\n");
  fprintf(fp,"        else\n");
  fprintf(fp,"          pfilename=optarg\n");
  fprintf(fp,"          got_pf=1\n");
  fprintf(fp,"        end if\n");
  fprintf(fp,"        i=i+1\n");
  fprintf(fp,"        goto 100\n");
  fprintf(fp,"      end if\n");
  fprintf(fp,"      if(argerr .eq. 1) then\n");
  fprintf(fp,"        write(*,*) 'Usage: %s'\n",oname);
  fprintf(fp,"        write(*,*) '     [ -p \"parameter:=value\" ]'\n");
  fprintf(fp,"        write(*,*) '     [ -v \"parallel_parameter:=fvec\" ]'\n");
  fprintf(fp,"        write(*,*) '     [ parameter_file ]'\n");
  fprintf(fp,"        stop\n");
  fprintf(fp,"      end if\n");
  fprintf(fp,"      if(got_pf .ne. 1) then\n");
  fprintf(fp,"101     continue \n");
  fprintf(fp,"        write(*,*) 'Enter name of parameter file: '\n");
  fprintf(fp,"        read(*,1010,end=101,err=101) pfilename\n");
  fprintf(fp,"1010    format(a)  \n");
  fprintf(fp,"      end if\n");
   fprintf(fp,"      if(parg.eq.1) then\n");
   fprintf(fp,"        call MPI_Comm_size(MPI_COMM_WORLD,mpisize,ierr)\n");
   fprintf(fp,"        call MPI_Comm_rank(MPI_COMM_WORLD,myproc,ierr)\n");
   fprintf(fp,"        psize=size_fvec(parpar)\n");
   fprintf(fp,"        num_groups=psize/mpisize\n");
   fprintf(fp,"        left_over=mod(psize,mpisize)\n");
   fprintf(fp,"        do i=1,num_groups\n");
   fprintf(fp,"          pval=get_fvec(parpar,(i-1)*mpisize+myproc)\n");
   fprintf(fp,"          pvals=' '\n");
   fprintf(fp,"          call realtostring(pval,pvals)\n");
   sprintf(forbuf,"          dirname='%s_'//",oname);
   fort_out(fp,forbuf);
   fort_out(fp,"parname(1:indlnb(parname))//");
   fort_out(fp,"'='//pvals(1:indlnb(pvals))\n");
   fprintf(fp,"          rc=mkdir(dirname(1:indlnb(dirname)),511)\n");
   fort_out(fp,"          syscom='cp .rnpl.attributes '//");
   fort_out(fp,"pfilename(1:indlnb(pfilename))//");
   fort_out(fp,"' '//dirname(1:indlnb(dirname))\n");
   fprintf(fp,"          call system(syscom)\n");
   fprintf(fp,"          rc=chdir(dirname(1:indlnb(dirname)))\n");
   fprintf(fp,"          flag='-p'\n");
   fprintf(fp,"          param=parname(1:indlnb(parname))//':='//pvals(1:indlnb(pvals))\n");
   fprintf(fp,"          myargv(argptr)=flag\n");
   fprintf(fp,"          myargv(argptr+1)=param\n");
   fprintf(fp,"          call sub_main(myargc,myargv)\n");
   fprintf(fp,"          rc=chdir('..')\n");
   fprintf(fp,"        end do\n");
   fprintf(fp,"        if(myproc.lt.left_over) then\n");
   fprintf(fp,"          pval=get_fvec(parpar,(i-1)*mpisize+myproc)\n");
   fprintf(fp,"          pvals=' '\n");
   fprintf(fp,"          call realtostring(pval,pvals)\n");
   fprintf(fp,"          dirname='%s_'//parname(1:indlnb(parname))//'='//pvals\n",oname);
   fprintf(fp,"          rc=mkdir(dirname(1:indlnb(dirname)),511)\n");
   fort_out(fp,"          syscom='cp .rnpl.attributes '");
   fort_out(fp,"//pfilename(1:indlnb(pfilename))");
   fort_out(fp,"//' '//dirname(1:indlnb(dirname))\n");
   fprintf(fp,"          call system(syscom)\n");
   fprintf(fp,"          rc=chdir(dirname(1:indlnb(dirname)))\n");
   fprintf(fp,"          flag='-p'\n");
   fprintf(fp,"          param=parname(1:indlnb(parname))//':='//pvals(1:indlnb(pvals))\n");
   fprintf(fp,"          myargv(argptr)=flag\n");
   fprintf(fp,"          myargv(argptr+1)=param\n");
   fprintf(fp,"          call sub_main(myargc,myargv)\n");
   fprintf(fp,"          rc=chdir('..')\n");
   fprintf(fp,"        end if\n");
   fprintf(fp,"      else\n");
   fprintf(fp,"        call sub_main(myargc,myargv)\n");
   fprintf(fp,"      end if\n");
   fprintf(fp,"      call MPI_Finalize(ierr)\n");
  fprintf(fp,"      stop\n");
  fprintf(fp,"\n");
  fprintf(fp,"      end\n");
  fprintf(fp,"\n");
}

void make_f77_main(FILE *fp, char *oname)
{
  if(parallel==TRIV){
     make_par_f77_main(fp,oname);
     fprintf(fp,"!-----------------------------------------------------------------------\n");
     fprintf(fp,"!\n");
     fprintf(fp,"!   This is the f77 sub_main\n");
     fprintf(fp,"!    \n");
     fprintf(fp,"!-----------------------------------------------------------------------\n");
     fprintf(fp,"\n");
     fprintf(fp,"      subroutine  sub_main(argc,argv)\n");
  }else if(parallel==0){
     fprintf(fp,"!-----------------------------------------------------------------------\n");
     fprintf(fp,"!\n");
     fprintf(fp,"!   This is the f77 main\n");
     fprintf(fp,"!    \n");
     fprintf(fp,"!-----------------------------------------------------------------------\n");
     fprintf(fp,"\n");
     fprintf(fp,"      program     main\n");
  }
  fprintf(fp,"\n");
  fprintf(fp,"      implicit       none\n");
  fprintf(fp,"\n");
  fprintf(fp,"      include       'sys_param.inc'\n");
  fprintf(fp,"      include       'gfuni0.inc'\n");
   fprintf(fp,"      integer argc\n");
   if(parallel==TRIV){
     fprintf(fp,"      character*64 argv(argc)\n");
   }else if(parallel==0){
      fprintf(fp,"      character*64 argv(64)\n");
   }
  fprintf(fp,"\n");
  fprintf(fp,"      include       'globals.inc'\n");
  fprintf(fp,"      include       'other_glbs.inc'\n");
  fprintf(fp,"\n");
  fprintf(fp,"      integer      iargc, getu, indlnb,do_out\n");
  fprintf(fp,"      integer      got_pf,argerr,do_ivec\n");
  fprintf(fp,"      integer      IVEL\n");
  fprintf(fp,"      parameter  ( IVEL = 4 )\n");
  fprintf(fp,"\n");
  fprintf(fp,"      character*64 param_file,optarg\n");
  fprintf(fp,"      integer      fp,i,j,steps,rc,ot,max,ret\n");
  fprintf(fp,"      real*8       t\n");
  fprintf(fp,"      integer      update_gfuncs, query_interrupt, check_params_attribs\n");
  fprintf(fp,"      \n");
  if(parallel==0){
     fprintf(fp,"      argc=iargc()\n");
     fprintf(fp,"      do i=1,argc\n");
     fprintf(fp,"        call getarg(i,argv(i))\n");
     fprintf(fp,"      end do\n");
  }
  fprintf(fp,"      call mmini(memsiz)\n");
  fprintf(fp,"      call init_params_attribs()\n");
  fprintf(fp,"      i=1\n");
  fprintf(fp,"      argerr=0\n");
  fprintf(fp,"      got_pf=0\n");
  fprintf(fp,"100   continue\n");
  fprintf(fp,"      if(i.le.argc) then\n");
  fprintf(fp,"        optarg=argv(i)\n");
  fprintf(fp,"        if(optarg .eq. '-p') then\n");
  fprintf(fp,"          i=i+1\n");
  fprintf(fp,"          optarg=argv(i)\n");
  fprintf(fp,"          call sread_parameters(optarg)\n");
  fprintf(fp,"        else if(optarg(1:1).eq.'-') then\n");
  fprintf(fp,"          argerr=1\n");
  fprintf(fp,"        else\n");
  fprintf(fp,"          param_file=optarg\n");
  fprintf(fp,"          got_pf=1\n");
  fprintf(fp,"        end if\n");
  fprintf(fp,"        i=i+1\n");
  fprintf(fp,"        goto 100\n");
  fprintf(fp,"      end if\n");
  fprintf(fp,"      if(argerr .eq. 1) then\n");
  fprintf(fp,"        write(*,*) 'Usage: %s'\n",oname);
  fprintf(fp,"        write(*,*) '     [ -p \"parameter:=value\" ]'\n");
  fprintf(fp,"        write(*,*) '     [ parameter_file ]'\n");
  fprintf(fp,"        stop\n");
  fprintf(fp,"      end if\n");
  fprintf(fp,"      if(got_pf .ne. 1) then\n");
  fprintf(fp,"101     continue \n");
  fprintf(fp,"        write(*,*) 'Enter name of parameter file: '\n");
  fprintf(fp,"        read(*,1010,end=101,err=101) param_file\n");
  fprintf(fp,"1010    format(a)  \n");
  fprintf(fp,"      end if\n");
  fprintf(fp,"      fp=getu()\n");
  fprintf(fp,"      open(unit=fp,file=param_file(1:indlnb(param_file)),\n");
  fprintf(fp,"     &     status = 'old', form = 'unformatted', iostat = rc)\n");
  fprintf(fp,"      if(rc .ne. 0) then\n");
  fprintf(fp,"        write(*,*) 'Unable to open initial data file '//\n");
  fprintf(fp,"     &             param_file(1:indlnb(param_file))\n");
  fprintf(fp,"        stop \n");
  fprintf(fp,"      end if\n");
  fprintf(fp,"      close(fp)\n");
  fprintf(fp,"      \n");
  fprintf(fp,"      call read_parameters(param_file(1:indlnb(param_file)))\n");
  fprintf(fp,"      call read_attributes(param_file(1:indlnb(param_file)))\n");
  fprintf(fp,"      call read_attributes('.rnpl.attributes')\n");
  fprintf(fp,"      call init_coord_difs()\n");
  fprintf(fp,"      call init_lats()\n");
  fprintf(fp,"      call init_gfuncs()\n");
  fprintf(fp,"      call read_state(argc,argv)\n");
  fprintf(fp,"      ret=check_params_attribs()\n");
  fprintf(fp,"      if(ret.eq.0) then\n");
  fprintf(fp,"        write(*,*) '%s: Unable to continue.'\n",oname);
  fprintf(fp,"        stop\n");
  fprintf(fp,"      end if\n");
  fprintf(fp,"\n");
  fprintf(fp,"      iter = iter * 2.0**level\n");
  fprintf(fp,"\n");
  fprintf(fp,"!------------------------------------------------------------------------\n");
  fprintf(fp,"!   set up signal handler \n");
  fprintf(fp,"!------------------------------------------------------------------------\n");
  fprintf(fp,"\n");
  fprintf(fp,"      call set_interrupt()\n");
  fprintf(fp," \n");
  fprintf(fp,"!------------------------------------------------------------------------\n");
  fprintf(fp,"!   start main loop\n");
  fprintf(fp,"!------------------------------------------------------------------------\n");
  fprintf(fp,"      rmod = -1\n");
  fprintf(fp,"      rnpldone = 0\n");
  fprintf(fp,"      t = start_t\n");
  fprintf(fp,"      i = s_step\n");
  fprintf(fp,"      if(set_epsiterid.eq.0) then\n");
  fprintf(fp,"        epsiterid=epsiter\n");
  fprintf(fp,"      end if\n");
  fprintf(fp,"      if(set_trace.eq.0) then\n");
  fprintf(fp,"        call rivcpy(trace,output,output(1)*IVEL+1)\n");
  fprintf(fp,"      end if\n");
  fprintf(fp,"!   fix ivec indicies\n");
  fprintf(fp,"      call fixup_ivec(0,iter,level,output)\n");
  fprintf(fp,"      call fixup_ivec(0,iter,level,trace)\n\n");
  fprintf(fp,"      if(i.eq.0) then\n");
  fprintf(fp,"        if(do_ivec(i,iter,trace).eq.1) then\n");
  fprintf(fp,"          write(*,*) 'Starting evolution.  step: ',i,' at t=',t\n");
  fprintf(fp,"        end if\n");
  fprintf(fp,"        if(((rmod.ne.-1).and.(mod(i,rmod).eq.0)).or.\n");
  fprintf(fp,"     &     (do_ivec(i,iter,output).eq.1)) then\n");
  fort_out(fp,"          ");
  output_call(fp);
  fprintf(fp,"        end if\n");
  fprintf(fp,"      end if\n");
  fprintf(fp,"      j=i+1\n");
  fprintf(fp,"      do i=j,iter\n");
  fprintf(fp,"        steps=update_gfuncs(t)\n");
  fprintf(fp,"        t = t+dt\n");
  fprintf(fp,"        if(do_ivec(i,iter,trace).eq.1) then\n");
  fprintf(fp,"          write(*,*) 'step: ',i,' t=',t, ' steps=',steps\n");
  fprintf(fp,"        end if\n");
  fprintf(fp,"        if(((rmod.ne.-1).and.(mod(i,rmod).eq.0)).or.\n");
  fprintf(fp,"     &     (do_ivec(i,iter,output).eq.1)) then\n");
  fort_out(fp,"          ");
  output_call(fp);
  fprintf(fp,"        end if\n");
  fprintf(fp,"        if(query_interrupt().ne.0) then\n");
  fprintf(fp,"          call handler()\n");
  fprintf(fp,"          call reset_interrupt()\n");
  fprintf(fp,"        end if\n");
  fprintf(fp,"        if(rnpldone .eq. 1) then\n");
  fprintf(fp,"          goto 9999\n");
  fprintf(fp,"        end if\n");
  fprintf(fp,"      end do\n");
  fprintf(fp,"9999  continue\n");
  fprintf(fp,"      call dump_state(t,i-1,out_file)\n");
  fprintf(fp,"      call cleanup()\n");
  if(parallel==0){
     fprintf(fp,"      stop\n");
     fprintf(fp,"\n");
  }
  fprintf(fp,"      end\n");
  fprintf(fp,"\n");
}

void make_iter_loop_c(FILE *fp)
{
  gfunc_ref_list *gl;
  coord_list *cl;
  offset_type *o;
  
  fprintf(fp,"int update_gfuncs(int *rnpldone, int maxstep, double epsiter,");
  update_iter_header_args(fp);
  fprintf(fp,")\n{\n");
  fprintf(fp,"  int i,j,k, step;\n");
  fprintf(fp,"  double rnplu=0.0;\n");
  fprintf(fp,"\n  ");
  /* initial_guess */
  for(gl=update_glob_gf;gl!=NULL;){
    if(gl->gfunc->ntlevs>1){
      o=gl->gfunc->tlev;
      fprintf(fp,"    rdvcpy(%s",gl->gfunc->fname->n);
      gfunc_suffix(fp,o->offset);
      fprintf(fp,",%s",gl->gfunc->fname->n);
      gfunc_suffix(fp,o->next->offset);
      fprintf(fp,",");
      grid_size(fp,gl->gfunc->grd);
      fprintf(fp,");\n");
    }
    while(gl->next && gl->next->gfunc==gl->gfunc) gl=gl->next;
    gl=gl->next;
  }

  fprintf(fp,"  step=0;\n");
  fprintf(fp,"  do{\n");
  fprintf(fp,"    step++;\n    ");
  take_step_call(fp);
  fprintf(fp,"    rnplu=");
  calc_resid_call(fp);
  fprintf(fp,"  }while(rnplu>epsiter && step<maxstep);\n");
  fprintf(fp,"  if(step>=maxstep)\n");
  fprintf(fp,"    fprintf(stdout,\"Update did not converge within %%d iterations.\\n\",maxstep);\n");
  fprintf(fp,"  return step;\n");
  fprintf(fp,"}\n");
}

void make_iter_loop_f77(FILE *fp)
{
  fprintf(fp,"      integer function update_gfuncs(t)\n");
  fprintf(fp,"        implicit none\n");
  fprintf(fp,"        \n");
  fprintf(fp,"        real*8   t\n");
  fprintf(fp,"        \n");
  fprintf(fp,"        include 'sys_param.inc'\n");
  fprintf(fp,"        include 'gfuni0.inc'\n");
  fprintf(fp,"        include 'globals.inc'\n");
  fprintf(fp,"        include 'other_glbs.inc'\n");
  fprintf(fp,"        \n");
  fprintf(fp,"        integer  step\n");
  fprintf(fp,"        real*8   u, calc_resid\n");
  fprintf(fp,"        \n");
  fprintf(fp,"        call initial_guess\n");
  fprintf(fp,"        step=0\n");
  fprintf(fp,"        u=0.0\n");
  fprintf(fp,"100       step=step+1\n");
  fprintf(fp,"          call take_step(t)\n");
  fprintf(fp,"          u=calc_resid()\n");
  fprintf(fp,"        if(u .ge. epsiter .and. step .lt. maxstep) then\n");
  fprintf(fp,"          goto 100\n");
  fprintf(fp,"        end if\n");
  fprintf(fp,"        call swap_levels\n");
  fprintf(fp,"        if(step .ge. maxstep) then\n");
  fprintf(fp,"          write(*,*) 'Update did not converge within ',maxstep,\n");
  fprintf(fp,"     &               ' iterations.'\n");
  fprintf(fp,"        end if\n");
  fprintf(fp,"        update_gfuncs=step\n");
  fprintf(fp,"        return\n");
  fprintf(fp,"      end\n");
  fprintf(fp,"      \n");
  fprintf(fp,"      \n");
}

void make_stand_loop_c(FILE *fp)
{
  fprintf(fp,"int update_gfuncs(int *rnpldone, int maxstep, double epsiter,");
  update_iter_header_args(fp);
  fprintf(fp,")\n{\n");
  fprintf(fp,"  ");
  take_step_call(fp);
  fprintf(fp,"  return 1;\n");
  fprintf(fp,"}\n");
}

void make_stand_loop_f77(FILE *fp)
{
  fprintf(fp,"      integer function update_gfuncs(t)\n");
  fprintf(fp,"        implicit none\n");
  fprintf(fp,"        include 'sys_param.inc'\n");
  fprintf(fp,"        include 'gfuni0.inc'\n");
  fprintf(fp,"        include 'globals.inc'\n");
  fprintf(fp,"        \n");
  fprintf(fp,"        real*8   t\n");
  fprintf(fp,"        \n");
  fprintf(fp,"        call take_step(t)\n");
  fprintf(fp,"        call swap_levels\n");
  fprintf(fp,"        update_gfuncs=1\n");
  fprintf(fp,"        return\n");
  fprintf(fp,"      end\n");
  fprintf(fp,"      \n");
  fprintf(fp,"      \n");
}

/* only for FORTRAN */
void output_gfuni0()
{
  FILE *fp;
  
  fp=fopen("gfuni0.inc","w");
  if(fp==NULL){
    fprintf(stderr,"RNPL: WARNING: Unable to open <gfuni0.inc> for output.\n");
    fprintf(stderr,"               Program may be unable to compile.\n");
    return;
  }
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"! M A I N   D A T A   S T R U C T U R E\n");
  fort_out(fp,"! For storing/manipulating arbitrary number of grid functions\n");
  fort_out(fp,"! defined on an arbitrary number of distinct lattices.\n");
  fort_out(fp,"! Non-parameter values stored in common block / com_gfuni0 /\n");
  fort_out(fp,"! although common facility may not actually be used.\n");
  fort_out(fp,"!----------------------------------------------------------------------\n");
  fort_out(fp,"      integer         getnlat,   getlatid,  getrank,  getpshape,\n");
  fort_out(fp,"     &                getshape,  getpcoord, getsize, getpbounds,\n");
  fort_out(fp,"     &                getbounds, getlatcs\n");
  fort_out(fp,"      external        getnlat,   getlatid,  getrank,  getpshape,\n");
  fort_out(fp,"     &                getshape,  getpcoord, getsize, getpbounds,\n");
  fort_out(fp,"     &                getbounds, getlatcs\n");
  fort_out(fp,"      real*8          q(memsiz)\n");
  fort_out(fp,"      common        / com_gfuni0 / q\n");
  fort_out(fp,"      integer         maxrank\n");
  fort_out(fp,"      parameter     ( maxrank = 5 )\n");
  fort_out(fp,"      integer         sizelatcb\n");
  fort_out(fp,"      parameter     ( sizelatcb = 3 + 4 * maxrank )\n");
  fort_out(fp,"      integer         maxgfcn\n");
  fort_out(fp,"      parameter     ( maxgfcn = 100 )\n");
  fort_out(fp,"      integer         ngfcn\n");
  fort_out(fp,"      common        / com_gfuni0 / ngfcn\n");
  fort_out(fp,"      integer         maxptrs\n");
  fort_out(fp,"      parameter     ( maxptrs = maxgfcn * (2 + sizelatcb) )\n");
  fort_out(fp,"      integer         ptrs(maxptrs)\n");
  fort_out(fp,"      common        / com_gfuni0 /  ptrs\n");
  fort_out(fp,"      integer         nptrs\n");
  fort_out(fp,"      common        / com_gfuni0 /  nptrs\n");
  fclose(fp);
}
