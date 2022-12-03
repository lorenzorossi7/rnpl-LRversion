%{
/* grammar for RNPL */
/* $Header: /home/cvs/rnpl/src/rnpl.y,v 1.1.1.1 2013/07/09 00:38:27 cvs Exp $ */
/* Copyright (c) 1994-1998 by Robert L. Marsa */

#define VERSION "0.995"

#include <stdlib.h>
#include "rnpl.h"

param_dec *pd;
coord_dec *cd;
grid_list *gd;
gfunc_list *gf;
attrib_dec *ad;
operator *od;
residual *rd;
init_dec *id;
name_list *ld;
update *ud,*iz;

void setup_tables();

extern FILE *rnplin;
%}

%union {
  int inum;
  double num;
  char op;
  d_op *deriv;
  func *fnc;
  i_reg *ireg;
  c_reg *creg;
  gfunction *gfunc;
  residual *resid;
  operator *doper;
  offset_type *olst;
  grid_list *grlst;
  gfunc_list *gfnlst;
  name_list *nml;
  coord_list *crd;
  coord_dec *crdec;
  update *updt;
  char *str;
  node *nd;
  v_size *vsz;
  param_dec *pdec;
  attrib_dec *atdec;
	attrib_set *atset;
  coord_ref *cdnt;
  scalar sclr;
  scalar_list *sclrlst;
  init_dec *indec;
  res_list *rlst;
  ref_list *rflst;
  indel *ndl;
  index_list *indl;
  ifstat *ifs;
  ivec_list *ivlst;
}

%token <str> STR IDEN
%token <op> OP
%token <num> NUM
%token <inum> INUM INT FLOAT COMPLEX STRING COORDINATES GRID ON PARAM AUTO STUB
%token <inum> RESID UPDATE C CPP F77 F90 ALLF UPF IDF UNIFORM NONUNIFORM AT ALIAS
%token <inum> OPERATOR QUOTE EXPAND FUNC D_OP GFUNC COORD STATIC CONST
%token <inum> COMMA OBRACK CBRACK OBRACE CBRACE OPAREN CPAREN COLON
%token <inum> ATTRIB ENCODEALL ENCODEONE ENCODERES INITIALIZE LOOPER EVALUATE
%token <inum> HEADER WORK POUND UPFILE CDIF GBASE SYS OFFS EXPR TIME
%token <inum> IF THEN ELSE IVEC RNPL DAGH TRIV
%token SCOLON

%type <str> aname
%type <doper> d_operator
%type <ireg> i_region
%type <nml> name looper
%type <crd> coord_list
%type <nd> expr
%type <resid> residual
%type <rlst> res_list
%type <gfunc> gfunc
%type <creg> c_region
%type <olst> o_list index
%type <inum> time type p_type g_type encoding ivel 
%type <grlst> grid_dec
%type <gfnlst> gfunc_dec
%type <crdec> coord_dec
%type <deriv> d_op
%type <fnc> func
%type <updt> update initializer
%type <vsz> v_size
%type <pdec> param_dec
%type <atdec> attrib_dec
%type <atset> attrib_set
%type <cdnt> coord
%type <sclr> scalar
%type <sclrlst> scalar_list vector
%type <indec> initialization
%type <rflst> ref_list reference
%type <ndl> indel
%type <indl> mindex
%type <ifs> ifstat
%type <ivlst> ivec_list
%type <op> becomes

%nonassoc <op> EQUALS ASSIGNOP
%left <op> OR
%left <op> AND
%nonassoc <op> EQUIV NOTEQ
%nonassoc <op> GREAT LESS GREATEQ LESSEQ
%left <op> PLUS MINUS
%left <op> TIMES DIVIDE MODULUS
%right UMINUS
%right <op> CARET

%%
dec_list: /* nothing */
  |  dec_list declaration
  ;
  
declaration:  param_dec {
                if(pd==NULL)
                  pd=$1;
                else add_param(pd,$1);
              }
  |  coord_dec {
      if(cd==NULL)
        cd=$1;
      else {
        $1->next=cd;
        cd=$1;
      }
    }
  |  grid_dec {
      if(gd==NULL)
        gd=$1;
      else {
        $1->next=gd;
        gd=$1;
      }
    }
  |  gfunc_dec {
      if(gf==NULL)
        gf=$1;
      else {
        $1->next=gf;
        gf=$1;
      }
    }
  |  attrib_dec {
      if(ad==NULL)
        ad=$1;
      else add_attrib(ad,$1);
    }
  |  d_operator {
      if(od==NULL)
        od=$1;
      else {
        $1->next=od;
        od=$1;
      }
    }
  | residual {
      if(rd==NULL)
        rd=$1;
      else {
        $1->next=rd;
        rd=$1;
      }
    }
  |  initialization {
      if(id==NULL)
        id=$1;
      else {
        $1->next=id;
        id=$1;
      }
    }
  |  initializer {
      if(iz==NULL)
        iz=$1;
      else {
        $1->next=iz;
        iz=$1;
      }
    }
  |  looper {
      if(ld==NULL)
        ld=$1;
      else rnplerror("Only one looper declaration is permitted.");
    }
  |  update {
      if(ud==NULL)
        ud=$1;
      else {
        $1->next=ud;
        ud=$1;
      }
    }
  ;
      
param_dec: PARAM p_type name {
             $$=new_param_dec();
             $$->type=$2;
             $$->size=new_v_size();
             $$->size->dim=1;
             $$->size->size=new_i_ar(1);
             $$->size->size[0]=1;
             $$->con=0;
             $$->name=$3;
             $$->def=0;
             switch($$->type){
               case FLOAT : $$->def_val.f_ar=new_f_ar(1);
                            break;
               case INT    : $$->def_val.i_ar=new_i_ar(1);
                            break;
               case STRING: $$->def_val.s_ar=new_s_ar(1);
                            break;
             }
           }
  | PARAM p_type name becomes scalar {
      $$=new_param_dec();
      $$->type=$2;
      $$->size=new_v_size();
      $$->size->dim=1;
      $$->size->size=new_i_ar(1);
      $$->size->size[0]=1;
      $$->con=0;
      $$->name=$3;
      $$->def=1;
      switch($$->type){
        case FLOAT : $$->def_val.f_ar=new_f_ar(1);
                     if($5.type==INT)
                       $$->def_val.f_ar[0]=(double)$5.data.inum;
                     else 
                       $$->def_val.f_ar[0]=$5.data.num;
                     break;
        case INT   : $$->def_val.i_ar=new_i_ar(1);
                     if($5.type==INT)
                       $$->def_val.i_ar[0]=$5.data.inum;
                     else {
                       fprintf(stderr,"parameter %s delared int, but assigned float.\n",$$->name->n);
                       rnplerror("Parameter Specification");
                     }
                     break;
        case STRING: $$->def_val.s_ar=new_s_ar(1);
                     if($5.type==STRING)
                       $$->def_val.s_ar[0]=$5.data.str;
                     else {
                       fprintf(stderr,"parameter %s delared string, but assigned a number.\n",$$->name->n);
                       rnplerror("Parameter Specification");
                     }
                     break;
      }
    }
  | PARAM p_type name v_size {
      $$=new_param_dec();
      $$->type=$2;
      $$->size=$4;
      $$->con=0;
      $$->name=$3;
      $$->def=0;
      switch($$->type){
        case FLOAT : $$->def_val.f_ar=new_f_ar(vsize($$->size));
                     break;
        case INT   : $$->def_val.i_ar=new_i_ar(vsize($$->size));
                     break;
        case STRING: $$->def_val.s_ar=new_s_ar(vsize($$->size));
                     break;
      }
    } 
  | PARAM p_type name v_size becomes vector {
      int i;
      scalar_list *p;
      $$=new_param_dec();
      $$->type=$2;
      $$->size=$4;
      $$->con=0;
      $$->name=$3;
      $$->def=1;
      switch($$->type){
        case FLOAT : $$->def_val.f_ar=new_f_ar(vsize($$->size));
                     for(p=$6,i=vsize($$->size)-1;i>=0;i--,p=p->next)
                       if(p->sc.type==FLOAT)
                         $$->def_val.f_ar[i]=p->sc.data.num;
                       else if(p->sc.type==INT)
                         $$->def_val.f_ar[i]=(double)p->sc.data.inum;
                       else rnplerror("bad parameter declaration");
                     break;
        case INT   : $$->def_val.i_ar=new_i_ar(vsize($$->size));
                     for(p=$6,i=vsize($$->size)-1;i>=0;i--,p=p->next)
                       if(p->sc.type==INT)
                         $$->def_val.i_ar[i]=p->sc.data.inum;
                       else rnplerror("bad parameter declaration");
                     break;
        case STRING: $$->def_val.s_ar=new_s_ar(vsize($$->size));
                     for(p=$6,i=vsize($$->size)-1;i>=0;i--,p=p->next)
                       if(p->sc.type==STRING)
                         $$->def_val.s_ar[i]=p->sc.data.str;
                       else rnplerror("bad parameter declaration");
                     break;
      }
    }
  |  PARAM IVEC name {
      $$=new_param_dec();
      $$->type=$2;
      $$->size=new_v_size();
      $$->size->dim=1;
      $$->size->size=new_i_ar(1);
      $$->size->size[0]=IVEL+1;
      $$->con=0;
      $$->name=$3;
      $$->def=0;
    }
  |  PARAM IVEC name becomes ivec_list {
      int c;
      ivec_list *p;
      $$=new_param_dec();
      $$->type=$2;
      $$->con=0;
      $$->name=$3;
      $$->def=1;
      for(c=0,p=$5;p!=NULL;p=p->next,c++);
      $$->def_val.i_ar=new_i_ar(IVEL*c+1);
      $$->def_val.i_ar[0]=c;
      $$->size=new_v_size();
      $$->size->dim=1;
      $$->size->size=new_i_ar(1);
      $$->size->size[0]=IVEL*c+1;
      for(c=1,p=$5;p!=NULL;p=p->next){
        $$->def_val.i_ar[c++]=p->low;
        $$->def_val.i_ar[c++]=p->high;
        $$->def_val.i_ar[c++]=p->mod;
        $$->def_val.i_ar[c++]=0;
      }
    }
  |  CONST PARAM p_type name {
      $$=new_param_dec();
      $$->type=$3;
      $$->size=new_v_size();
      $$->size->dim=1;
      $$->size->size=new_i_ar(1);
      $$->size->size[0]=1;
      $$->con=1;
      $$->name=$4;
      $$->def=0;
      switch($$->type){
        case FLOAT : $$->def_val.f_ar=new_f_ar(1);
                    break;
        case INT    : $$->def_val.i_ar=new_i_ar(1);
                    break;
        case STRING: $$->def_val.s_ar=new_s_ar(1);
                    break;
      }
    }
  | CONST PARAM p_type name becomes scalar {
      $$=new_param_dec();
      $$->type=$3;
      $$->size=new_v_size();
      $$->size->dim=1;
      $$->size->size=new_i_ar(1);
      $$->size->size[0]=1;
      $$->con=1;
      $$->name=$4;
      $$->def=1;
      switch($$->type){
        case FLOAT : $$->def_val.f_ar=new_f_ar(1);
                     if($6.type==INT)
                       $$->def_val.f_ar[0]=(double)$6.data.inum;
                     else 
                       $$->def_val.f_ar[0]=$6.data.num;
                     break;
        case INT   : $$->def_val.i_ar=new_i_ar(1);
                     if($6.type==INT)
                       $$->def_val.i_ar[0]=$6.data.inum;
                     else {
                       fprintf(stderr,"parameter %s delared int, but assigned float.\n",$$->name->n);
                       rnplerror("Parameter Specification");
                     }
                     break;
        case STRING: $$->def_val.s_ar=new_s_ar(1);
                     if($6.type==STRING)
                       $$->def_val.s_ar[0]=$6.data.str;
                     else {
                       fprintf(stderr,"parameter %s delared string, but assigned a number.\n",$$->name->n);
                       rnplerror("Parameter Specification");
                     }
                     break;
      }
    }
  | CONST PARAM p_type name v_size {
      $$=new_param_dec();
      $$->type=$3;
      $$->size=$5;
      $$->con=1;
      $$->name=$4;
      $$->def=0;
      switch($$->type){
        case FLOAT : $$->def_val.f_ar=new_f_ar(vsize($$->size));
                     break;
        case INT   : $$->def_val.i_ar=new_i_ar(vsize($$->size));
                     break;
        case STRING: $$->def_val.s_ar=new_s_ar(vsize($$->size));
                     break;
      }
    } 
  | CONST PARAM p_type name v_size becomes vector {
      int i;
      scalar_list *p;
      $$=new_param_dec();
      $$->type=$3;
      $$->size=$5;
      $$->con=1;
      $$->name=$4;
      $$->def=1;
      switch($$->type){
        case FLOAT : $$->def_val.f_ar=new_f_ar(vsize($$->size));
                     for(p=$7,i=vsize($$->size)-1;i>=0;i--,p=p->next)
                       if(p->sc.type==FLOAT)
                         $$->def_val.f_ar[i]=p->sc.data.num;
                       else if(p->sc.type==INT)
                         $$->def_val.f_ar[i]=(double)p->sc.data.inum;
                       else rnplerror("bad parameter declaration");
                     break;
        case INT   : $$->def_val.i_ar=new_i_ar(vsize($$->size));
                     for(p=$7,i=vsize($$->size)-1;i>=0;i--,p=p->next)
                       if(p->sc.type==INT)
                         $$->def_val.i_ar[i]=p->sc.data.inum;
                       else rnplerror("bad parameter declaration");
                     break;
        case STRING: $$->def_val.s_ar=new_s_ar(vsize($$->size));
                     for(p=$7,i=vsize($$->size)-1;i>=0;i--,p=p->next)
                       if(p->sc.type==STRING)
                         $$->def_val.s_ar[i]=p->sc.data.str;
                       else rnplerror("bad parameter declaration");
                     break;
      }
    }
  | SYS PARAM p_type name becomes scalar {
      $$=new_param_dec();
      $$->type=$3;
      $$->size=new_v_size();
      $$->size->dim=1;
      $$->size->size=new_i_ar(1);
      $$->size->size[0]=1;
      $$->con=2;
      $$->name=$4;
      $$->def=1;
      switch($$->type){
        case FLOAT : $$->def_val.f_ar=new_f_ar(1);
                     if($6.type==INT)
                       $$->def_val.f_ar[0]=(double)$6.data.inum;
                     else 
                       $$->def_val.f_ar[0]=$6.data.num;
                     break;
        case INT   : $$->def_val.i_ar=new_i_ar(1);
                     if($6.type==INT)
                       $$->def_val.i_ar[0]=$6.data.inum;
                     else {
                       fprintf(stderr,"parameter %s delared int, but assigned float.\n",$$->name->n);
                       rnplerror("Parameter Specification");
                     }
                     break;
        case STRING: $$->def_val.s_ar=new_s_ar(1);
                     if($6.type==STRING)
                       $$->def_val.s_ar[0]=$6.data.str;
                     else {
                       fprintf(stderr,"parameter %s delared string, but assigned a number.\n",$$->name->n);
                       rnplerror("Parameter Specification");
                     }
                     break;
      }
    }
  ;

coord_dec: name COORDINATES coord_list { 
             coord_list *p;
             $$=new_coord_dec();
             $$->name=$1;
             for($$->rank=0,p=$3;p!=NULL;p=p->next,$$->rank++);
             $$->c_names=$3;
           }
  ;
  
grid_dec: g_type name GRID name i_region c_region {
            $$=new_grid_list();
            $$->type=$1;
            $$->cname=$2;
            $$->name=$4;
            $$->ireg=$5;
            $$->creg=$6;
          }
  | g_type name GRID name {
      $$=new_grid_list();
      $$->type=$1;
      $$->cname=$2;
      $$->name=$4;
    }
  |  g_type name OBRACK coord_list CBRACK GRID name i_region c_region {
      $$=new_grid_list();
      $$->type=$1;
      $$->cname=$2;
      $$->clst=$4;
      $$->name=$7;
      $$->ireg=$8;
      $$->creg=$9;
    }
  | g_type name OBRACK coord_list CBRACK GRID name {
      $$=new_grid_list();
      $$->type=$1;
      $$->cname=$2;
      $$->clst=$4;
      $$->name=$7;
    }
  ;
  
gfunc_dec:  type name ON name {
              $$=new_gfunc_list();
              $$->type=$1;
              $$->alias=0;
              $$->fname=$2;
              $$->gname=$4;
            }
  |  type name ON name STR {
      $$=new_gfunc_list();
      $$->type=$1;
      $$->alias=0;
      $$->fname=$2;
      $$->gname=$4;
      $$->desc=$5;
    }
  |  type name ON name AT o_list {
      $$=new_gfunc_list();
      $$->type=$1;
      $$->alias=0;
      $$->fname=$2;
      $$->gname=$4;
      $$->tlev=$6;
    }
  | type name ON name AT o_list ALIAS {
      $$=new_gfunc_list();
      $$->type=$1;
      $$->fname=$2;
      $$->gname=$4;
      $$->tlev=$6;
      $$->alias=1;
    }
  | type name ON name AT o_list STR {
      $$=new_gfunc_list();
      $$->type=$1;
      $$->alias=0;
      $$->fname=$2;
      $$->gname=$4;
      $$->tlev=$6;
      $$->desc=$7;
    }
  | type name ON name AT o_list ALIAS STR {
      $$=new_gfunc_list();
      $$->type=$1;
      $$->fname=$2;
      $$->gname=$4;
      $$->tlev=$6;
      $$->alias=1;
      $$->desc=$8;
    }
	|	type name ON name OBRACE attrib_set CBRACE {
			$$=new_gfunc_list();
			$$->type=$1;
			$$->alias=0;
			$$->fname=$2;
			$$->gname=$4;
			$$->atts=$6;
		}
  | type name ON name STR OBRACE attrib_set CBRACE {
      $$=new_gfunc_list();
      $$->type=$1;
      $$->alias=0;
      $$->fname=$2;
      $$->gname=$4;
      $$->desc=$5;
			$$->atts=$7;
    }
  |  type name ON name AT o_list OBRACE attrib_set CBRACE {
      $$=new_gfunc_list();
      $$->type=$1;
      $$->alias=0;
      $$->fname=$2;
      $$->gname=$4;
      $$->tlev=$6;
			$$->atts=$8;
    }
  | type name ON name AT o_list ALIAS OBRACE attrib_set CBRACE {
      $$=new_gfunc_list();
      $$->type=$1;
      $$->fname=$2;
      $$->gname=$4;
      $$->tlev=$6;
      $$->alias=1;
			$$->atts=$9;
    }
  | type name ON name AT o_list STR OBRACE attrib_set CBRACE {
      $$=new_gfunc_list();
      $$->type=$1;
      $$->alias=0;
      $$->fname=$2;
      $$->gname=$4;
      $$->tlev=$6;
      $$->desc=$7;
			$$->atts=$9;
    }
  | type name ON name AT o_list ALIAS STR OBRACE attrib_set CBRACE {
      $$=new_gfunc_list();
      $$->type=$1;
      $$->fname=$2;
      $$->gname=$4;
      $$->tlev=$6;
      $$->alias=1;
      $$->desc=$8;
			$$->atts=$10;
    }
  ;
  
attrib_dec: ATTRIB p_type name encoding {
             $$=new_attrib_dec();
             $$->type=$2;
             $$->encoding=$4;
             $$->name=$3;
             $$->def=0;
           }
  | ATTRIB p_type name encoding becomes vector {
            $$=new_attrib_dec();
            $$->type=$2;
            $$->encoding=$4;
            $$->name=$3;
            $$->def=1;
            $$->def_val=$6;
          }
  ;

d_operator: OPERATOR d_op becomes expr {
              $$=new_operator();
              $$->deriv=$2;
              $$->op_expr=$4;
            }
  ;
  
residual: RESID name OBRACE res_list opcolon CBRACE {
            $$=new_residual();
            $$->eval=0;
            $$->name=$2;
            $$->res=$4;
          }
  |  RESID time index name OBRACE res_list opcolon CBRACE {
      $$=new_residual();
      $$->eval=0;
      $$->toff=$2;
      $$->indx=$3;
      $$->name=$4;
      $$->res=$6;
    }
  | EVALUATE RESID name OBRACE res_list opcolon CBRACE {
      $$=new_residual();
      $$->eval=1;
      $$->name=$3;
      $$->res=$5;
    }
  |  EVALUATE RESID time index name OBRACE res_list opcolon CBRACE {
      $$=new_residual();
      $$->eval=1;
      $$->toff=$3;
      $$->indx=$4;
      $$->name=$5;
      $$->res=$7;
    }      
  ;
  
initialization: INITIALIZE name OBRACE res_list opcolon CBRACE {
                  $$=new_init_dec();
                  $$->toff=0;
                  $$->name=$2;
                  $$->init=$4;
                }
	| INITIALIZE time name OBRACE res_list opcolon CBRACE {
      $$=new_init_dec();
      $$->toff=$2;
      $$->name=$3;
      $$->init=$5;
    }
  ;

initializer: name name INITIALIZE coord_list HEADER ref_list {
          $$=new_update();
          $$->type=$1;
          $$->name=$2;
          $$->gfs=$4;
          $$->refs=$6;
        } 
  |  STUB name INITIALIZE coord_list HEADER ref_list {
      $$=new_update();
      $$->type=name_lookup("stub");
      $$->name=$2;
      $$->gfs=$4;
      $$->refs=$6;
    }
  |  AUTO INITIALIZE coord_list {
      $$=new_update();
      $$->type=name_lookup("auto");
      $$->gfs=$3;
    }
  ;

looper:  LOOPER name {
          $$=$2;
        }
  ;
  
update: name name UPDATE coord_list HEADER ref_list {
          $$=new_update();
          $$->type=$1;
          $$->name=$2;
          $$->gfs=$4;
          $$->refs=$6;
        } 
  |  STUB name UPDATE coord_list HEADER ref_list {
      $$=new_update();
      $$->type=name_lookup("stub");
      $$->name=$2;
      $$->gfs=$4;
      $$->refs=$6;
    }
  |  AUTO UPDATE coord_list {
      $$=new_update();
      $$->type=name_lookup("auto");
      $$->gfs=$3;
    }
  ;

p_type: INT { $$=$1; }
  | FLOAT { $$=$1; }
  | STRING { $$=$1; }
  ;
  
name: IDEN {
        $$=add_name($1);
				free($1);
      }
  ;

scalar: INUM { 
          $$.type=INT;
          $$.data.inum=$1;
        }
  | MINUS INUM {
      $$.type=INT;
      $$.data.inum=-$2;
    }
  | NUM {
      $$.type=FLOAT;
      $$.data.num=$1;
    }
  | MINUS NUM {
      $$.type=FLOAT;
      $$.data.num=-$2;
    }
  | STR {
      $$.type=STRING;
      $$.data.str=$1;
    }
  ;

v_size: OBRACK INUM CBRACK {
      $$=new_v_size();
      $$->dim=1;
      $$->size=new_i_ar(1);
      $$->size[0]=$2;
    }
  |  v_size OBRACK INUM CBRACK {
      int i;
      
      $$=new_v_size();
      $$->dim=$1->dim+1;
      $$->size=new_i_ar($$->dim);
      for(i=0;i<$1->dim;i++)
        $$->size[i]=$1->size[i];
      $$->size[i]=$3;
      free($1->size);
      free($1);
    }
  ;

becomes: ASSIGNOP | EQUALS
  ;
  
vector: OBRACK scalar_list CBRACK { 
      $$=$2;
    }
  |  vector OBRACK scalar_list CBRACK {
      scalar_list *p;
      
      for(p=$3;p->next!=NULL;p=p->next);
      p->next=$1;
      $$=$3;
    }
  ;

ivec_list:  ivel {
              $$=new_ivec_list();
              $$->low=$1;
              $$->high=$1;
              $$->mod=1;
            }
  |  ivel MINUS ivel {
      $$=new_ivec_list();
      $$->low=$1;
      $$->high=$3;
      $$->mod=1;
    }
  |  ivel MINUS ivel DIVIDE INUM {
      $$=new_ivec_list();
      $$->low=$1;
      $$->high=$3;
      $$->mod=$5;
    }
  |  ivec_list COMMA ivel {
      ivec_list *p;
      for(p=$1;p->next!=NULL;p=p->next);
      p->next=new_ivec_list();
      p->next->low=$3;
      p->next->high=$3;
      p->next->mod=1;
      $$=$1;
    }
  |  ivec_list COMMA ivel MINUS ivel {
      ivec_list *p;
      for(p=$1;p->next!=NULL;p=p->next);
      p->next=new_ivec_list();
      p->next->low=$3;
      p->next->high=$5;
      p->next->mod=1;
      $$=$1;
    }
  | ivec_list COMMA ivel MINUS ivel DIVIDE INUM {
      ivec_list *p;
      for(p=$1;p->next!=NULL;p=p->next);
      p->next=new_ivec_list();
      p->next->low=$3;
      p->next->high=$5;
      p->next->mod=$7;
      $$=$1;
    }
  ;
  
coord_list: name { 
              $$=new_coord_list();
              $$->name=$1;
            }
  | coord_list COMMA name {
      coord_list *p;
      for(p=$1;p->next!=NULL;p=p->next);
      p->next=new_coord_list();
      p->next->name=$3; 
      $$=$1; 
    }
  ;

g_type: UNIFORM { $$=$1; }
  | NONUNIFORM { $$=$1; }
  ;
  
i_region: OBRACK expr COLON expr CBRACK { 
            $$=new_i_reg();
            $$->lower=$2;
            $$->upper=$4;
            $$->inc=1;
          }
  | OBRACK expr COLON expr CBRACK i_region { 
      $$=new_i_reg();
      $$->lower=$2;
      $$->upper=$4;
      $$->next=$6;
      $$->inc=1;
    }
  |  OBRACK expr COLON expr COLON INUM CBRACK { 
            $$=new_i_reg();
            $$->lower=$2;
            $$->upper=$4;
            $$->inc=$6;
    }
  |  OBRACK expr COLON expr COLON MINUS INUM CBRACK { 
            $$=new_i_reg();
            $$->lower=$2;
            $$->upper=$4;
            $$->inc=-$7;
    }
  | OBRACK expr COLON expr COLON INUM CBRACK i_region { 
      $$=new_i_reg();
      $$->lower=$2;
      $$->upper=$4;
      $$->inc=$6;
      $$->next=$8;
    }
  | OBRACK expr COLON expr COLON MINUS INUM CBRACK i_region { 
      $$=new_i_reg();
      $$->lower=$2;
      $$->upper=$4;
      $$->inc=-$7;
      $$->next=$9;
    }
  ;

c_region: OBRACE name COLON name CBRACE {
            $$=new_c_reg();
            $$->lower=$2; 
            $$->upper=$4; 
          }
  | OBRACE name COLON name CBRACE c_region{ 
      $$=new_c_reg();
      $$->lower=$2;
      $$->upper=$4;
      $$->next=$6;
    }
  ;
  
type: INT { $$=$1; }
  | FLOAT { $$=$1; }
  | COMPLEX { $$=$1; }
  ;

o_list: INUM { 
          $$=new_offset();
          $$->offset=$1;
         }
  | MINUS INUM { 
      $$=new_offset();
      $$->offset=-$2;
    }
  | o_list COMMA INUM {
      offset_type *os;
      
      os=new_offset();
      os->offset=$3;
      add_offset($1,os);
      $$=$1;
    }
  | o_list COMMA MINUS INUM { 
      offset_type *os;
      
      os=new_offset();
      os->offset=-$4;
      add_offset($1,os);
      $$=$1;
    }
  ;
  
attrib_set: aname becomes scalar {
			$$=new_attrib_set();
			$$->name=$1;
			$$->val=$3;
		}
	|	attrib_set COMMA aname becomes scalar {
			$$=new_attrib_set();
			$$->name=$3;
			$$->val=$5;
			$$->next=$1;
		}
	;
	
encoding: ENCODEONE { $$=$1; }
  | ENCODEALL { $$=$1; }
  ;

d_op: name OPAREN expr COMMA coord_list CPAREN { 
        coord_list *p;
        $$=new_dop();
        $$->expand=FALSE;
        $$->name=$1;
        $$->expr=$3;
        $$->clst=new_coord_tab();
        for($$->clst->rank=0,p=$5;p!=NULL;p=p->next,$$->clst->rank++);
        $$->clst->c_names=$5;
      }
  | EXPAND name OPAREN expr COMMA coord_list CPAREN {
      coord_list *p;
      $$=new_dop();
      $$->expand=TRUE;
      $$->name=$2;
      $$->expr=$4;
      $$->clst=new_coord_tab();
      for($$->clst->rank=0,p=$6;p!=NULL;p=p->next,$$->clst->rank++);
      $$->clst->c_names=$6;
    }
  ;
  
expr: expr PLUS expr {
        $$=new_node();
        $$->type=OP;
        $$->data.op=$2;
        $$->left=$1;
        $$->right=$3;
      }
  | expr MINUS expr {
      $$=new_node();
      $$->type=OP;
      $$->data.op=$2;
      $$->left=$1;
      $$->right=$3;
    }
  | expr EQUALS expr {
      $$=new_node();
      $$->type=OP;
      $$->data.op='-';
      $$->left=$1;
      $$->right=$3;
    }
  | expr TIMES expr {
      $$=new_node();
      $$->type=OP;
      $$->data.op=$2;
      $$->left=$1;
      $$->right=$3;
    }
  | expr DIVIDE expr {
      $$=new_node();
      $$->type=OP;
      $$->data.op=$2;
      $$->left=$1;
      $$->right=$3;
    }
  | expr MODULUS expr {
      $$=new_node();
      $$->type=OP;
      $$->data.op=$2;
      $$->left=$1;
      $$->right=$3;
    }
  | expr CARET expr {
      $$=new_node();
      $$->type=OP;
      $$->data.op=$2;
      $$->left=$1;
      $$->right=$3;
    }
  | PLUS expr %prec UMINUS {
      $$=new_node();
      $$->type=OP;
      $$->data.op=$1;
      $$->right=$2;
    }
  | MINUS expr %prec UMINUS {
      $$=new_node();
      $$->type=OP;
      $$->data.op=$1;
      $$->right=$2;
    }
  | expr EQUIV expr {
      $$=new_node();
      $$->type=OP;
      $$->data.op=$2;
      $$->left=$1;
      $$->right=$3;
    }
  | expr NOTEQ expr {
      $$=new_node();
      $$->type=OP;
      $$->data.op=$2;
      $$->left=$1;
      $$->right=$3;
    }
  | expr LESS expr {
      $$=new_node();
      $$->type=OP;
      $$->data.op=$2;
      $$->left=$1;
      $$->right=$3;
    }
  | expr GREAT expr {
      $$=new_node();
      $$->type=OP;
      $$->data.op=$2;
      $$->left=$1;
      $$->right=$3;
    }
  | expr LESSEQ expr {
      $$=new_node();
      $$->type=OP;
      $$->data.op=$2;
      $$->left=$1;
      $$->right=$3;
    }
  | expr GREATEQ expr {
      $$=new_node();
      $$->type=OP;
      $$->data.op=$2;
      $$->left=$1;
      $$->right=$3;
    }
  | expr AND expr {
      $$=new_node();
      $$->type=OP;
      $$->data.op=$2;
      $$->left=$1;
      $$->right=$3;
    }
  | expr OR expr {
      $$=new_node();
      $$->type=OP;
      $$->data.op=$2;
      $$->left=$1;
      $$->right=$3;
    }
  | OPAREN expr CPAREN {
      $$=$2;
    }
  | d_op {
      $$=new_node();
      $$->type=D_OP;
      $$->data.deriv=$1;
    }
  | func {
      $$=new_node();
      $$->type=FUNC;
      $$->data.func=$1;
    }
  | gfunc {
      $$=new_node();
      $$->type=GFUNC;
      $$->data.gfunc=$1;
    }
  | coord { 
      $$=new_node();
      $$->type=COORD; 
      $$->data.coord=$1;
    }
  | name { 
      $$=new_node();
      $$->type=IDEN; 
      $$->data.name=$1;
    }
  | NUM { 
      $$=new_node();
      $$->type=NUM; 
      $$->data.num=$1;
    }
  | INUM { 
      $$=new_node();
      $$->type=INUM; 
      $$->data.inum=$1;
    }
  ;
  
res_list:  i_region becomes expr {
            $$=new_res_list();
            $$->reg=$1;
            $$->etype=1;
            $$->elst.expr=$3;
          }
  |  res_list SCOLON i_region becomes expr {
      $$=new_res_list();
      $$->reg=$3;
      $$->etype=1;
      $$->elst.expr=$5;
      $$->next=$1;
    }
  |  i_region becomes ifstat {
            $$=new_res_list();
            $$->reg=$1;
            $$->etype=2;
            $$->elst.ifst=$3;
          }
  |  res_list SCOLON i_region becomes ifstat {
      $$=new_res_list();
      $$->reg=$3;
      $$->etype=2;
      $$->elst.ifst=$5;
      $$->next=$1;
    }
  ;
  
opcolon: /* nothing */
  |  SCOLON
  ;
  
time: TIME { $$=$1; }
  ;
  
index: OBRACK INUM CBRACK { 
         $$=new_offset();
         $$->offset=$2;
       }
  | OBRACK MINUS INUM CBRACK { 
      $$=new_offset();
      $$->offset=-$3;
    }
  | OBRACK INUM CBRACK index { 
      $$=new_offset();
      $$->offset=$2;
      $$->next=$4;
    }
  | OBRACK MINUS INUM CBRACK index { 
      $$=new_offset();
      $$->offset=-$3;
      $$->next=$5;
    }
  ;

ref_list: reference {
            $$=$1;
          }
  |  ref_list COMMA reference {
      $$=$3;
      $$->next=$1;
    }
  ;
  
scalar_list: scalar {
               $$=new_scalar_list();
               $$->sc=$1;
             }
  | scalar_list scalar {
      $$=new_scalar_list();
      $$->sc=$2;
      $$->next=$1;
    }
  ;

ivel:  INUM { $$=$1; }
  |  TIMES { $$=-1; }
  ;
  
func: name OPAREN expr CPAREN {
        $$=new_func();
        $$->name=$1;
        $$->expr=$3;
      }
  ;
  
gfunc: time name mindex { 
         $$=new_gfunc();
         $$->toff=$1;
         $$->name=$2;
         $$->indx=$3;
       }
  ;
    
coord: name indel {
         $$=new_coord();
         $$->name=$1;
         $$->indx=$2;
       }
  ;

ifstat:  IF expr THEN expr {
          $$=new_ifstat();
          $$->lexpr=$2;
          $$->expr=$4;
          $$->etype=0;
        }
  |  IF expr THEN expr ELSE expr {
      $$=new_ifstat();
      $$->lexpr=$2;
      $$->expr=$4;
      $$->etype=1;
      $$->elst.expr=$6;
    }
  |  IF expr THEN expr ELSE ifstat {
      $$=new_ifstat();
      $$->lexpr=$2;
      $$->expr=$4;
      $$->etype=2;
      $$->elst.ifst=$6;
    }
  ;
  
reference:  name {
              $$=new_ref_list();
              $$->type=IDEN;
              $$->data.nref=new_name_ref();
              $$->data.nref->name=$1;
            }
  |  name OBRACK coord_list CBRACK {
      $$=new_ref_list();
      $$->type=IDEN;
      $$->data.nref=new_name_ref();
      $$->data.nref->name=$1;
      $$->data.nref->subs=$3;
    }
  |  AUTO WORK POUND INUM OPAREN expr CPAREN {
      $$=new_ref_list();
      $$->type=WORK;
      $$->data.work=new_work_array();
      $$->data.work->ato=1;
      if($4<0) $4=-$4;
      $$->data.work->num=$4;
      $$->data.work->expr=$6;
    }
  |  STATIC WORK POUND INUM OPAREN expr CPAREN {
      $$=new_ref_list();
      $$->type=WORK;
      $$->data.work=new_work_array();
      $$->data.work->ato=0;
      if($4<0) $4=-$4;
      $$->data.work->num=$4;
      $$->data.work->expr=$6;
    }
  ;
  
mindex:  indel {
          $$=new_index_list();
          $$->ind=$1;
        }
  |  mindex indel {
      index_list *p;
      for(p=$1;p->next!=NULL;p=p->next);
      p->next=new_index_list();
      p->next->ind=$2;
      $$=$1;
    }
  ;
  
indel:  OBRACK INUM CBRACK {
          $$=new_indel();
          $$->type=OFFS;
          $$->val.offset=$2;
        }
  |  OBRACK MINUS INUM CBRACK {
      $$=new_indel();
      $$->type=OFFS;
      $$->val.offset=-$3;
    }
  |  OBRACE expr CBRACE {
      $$=new_indel();
      $$->type=EXPR;
      $$->val.expr=$2;
    }
  ;
	
aname:	IDEN {
					$$=$1;
				}
	| IDEN OBRACK IDEN CBRACK {
			$$=new_string(strlen($1)+strlen($3)+3);
			sprintf($$,"%s[%s]",$1,$3);
			free($1);
			free($3);
		}
	| IDEN OBRACK INUM CBRACK {
			$$=new_string(strlen($1)+23);
			sprintf($$,"%s[%d]",$1,$3);
			free($1);
		}
	| IDEN OBRACK MINUS INUM CBRACK {
			$$=new_string(strlen($1)+24);
			$4=-$4;
			sprintf($$,"%s[%d]",$1,$4);
			free($1);
		}
	;
  
%%

main(int argc, char **argv)
{
  int fn,rn,gn,i,err;
  char *fnm,ch,line[1024],oname[100],iname[50],ifile[100],*ev;
  coord_list *co,*nm;
  i_reg *ir;
  c_reg *cr;
  statement_list *stl;
  FILE *fpin,*fpout,*fpout_init,*fpout_inc,*fpout_inc1,*fpout_upd;
  FILE *fpout_initer,*fpout_res;
  name_list *n;
  int ostd=0,argerr=0,opt,oopt=0,lang=0;
  extern char *optarg;
  extern int optind,opterr,optopt;
  name_list *nlp;

  while((opt=getopt(argc,argv,"a:c:l:o:p:"))!=EOF){
    switch(opt){
      case 'a' :
        if(!strcmp(optarg,"RNPL"))
          adapt=RNPL;
        else if(!strcmp(optarg,"DAGH"))
          adapt=DAGH;
        else {
          fprintf(stderr,"Invalid adaptive option <%s>\n",optarg);
          argerr=1;
        }
        break;
      case 'c' :
        if(!strcmp(optarg,"sufD"))
          fort_const_D=1;
        else if(!strcmp(optarg,"sufE"))
          fort_const_D=0;
        else{
          fprintf(stderr,"Invalid FORTRAN constant option <%s>\n",optarg);
          fprintf(stderr,"Defaulting to <sufD>\n");
          fort_const_D=1;
        }
        break;
      case 'l' :
        if(!strcmp(optarg,"c")){
          language=C;
        }else if(!strcmp(optarg,"c++")){
          language=CPP;
        }else if(!strcmp(optarg,"f77")){
          language=F77;
        }else if(!strcmp(optarg,"f90")){
          language=F90;
        }else if(!strcmp(optarg,"allf")){
        	language=ALLF;
        }else if(!strcmp(optarg,"upf")){
        	language=UPF;
        }else if(!strcmp(optarg,"idf")){
        	language=IDF;
        }else{
          fprintf(stderr,"Invalid language <%s>\n",optarg);
          argerr=1;
        }
        lang=1;
        break;
      case 'o' :
        if(optarg[0]=='-'){
          fprintf(stderr,"Invalid output file name <%s>\n",optarg);
          argerr=1;
        }else{
          strcpy(oname,optarg);
          oopt=1;
        }
        break;
      case 'p' :
      	if(!strcmp(optarg,"triv")){
      		parallel=TRIV;
      	}else if(!strcmp(optarg,"DAGH")){
        	parallel=DAGH;
        }else{
        	fprintf(stderr,"Invalid parallel option <%s>\n",optarg);
        	argerr=1;
        }
        break;
      case '?' :
        argerr=1;
        break;
    }
  }
  if(argerr || !lang){
    fprintf(stderr,"  RNPL version: %s\n",VERSION);
    fprintf(stderr,"  Compiler Copyright (c) 1994-1998 by Robert L. Marsa\n");
    fprintf(stderr,"  Language Copyright (c) 1994-1998 by Robert L. Marsa and Matthew W. Choptuik\n\n");
    fprintf(stderr,"Usage: \n");
    fprintf(stderr,"  rnpl   -l { c | c++ | f77 | f90 | allf | idf | upf }\n");
    fprintf(stderr,"       [ -c { sufD | sufE } ]\n");
    fprintf(stderr,"       [ -o output_stem ]\n");
    fprintf(stderr,"       [ -a { RNPL | DAGH } ]\n"); 
    fprintf(stderr,"       [ -p { DAGH | triv } ]\n");
    fprintf(stderr,"       [ input_file ]\n\n");
    fprintf(stderr,"  -l selects the output language\n");
    fprintf(stderr,"  -c selects whether FORTRAN constants are written as D0 or E0\n");
    fprintf(stderr,"     (defaults to sufD)\n");
    fprintf(stderr,"  -o selects stem of output file names\n");
    fprintf(stderr,"     (defaults to the same as the input file stem)\n");
    fprintf(stderr,"  -a selects which system will be used to produce the\n");
    fprintf(stderr,"     adaptive driver if adaptivity is indicated in the source\n");
    fprintf(stderr,"  -p turns on parallel output and selects kind of parallelism\n");
    fprintf(stderr,"  If no input_file is specified, the compiler will read from\n");
    fprintf(stderr,"  stdin.  If in addition, no output stem is specified, the compiler\n");
    fprintf(stderr,"  will send the evolution code to stdout, while the initial data\n");
    fprintf(stderr,"  code will be written to <r_out_init>.\n\n");
    exit(0);
  }
  if(optind<argc){
    fpin=fopen(argv[optind],"r");
    if(fpin==NULL){
      fprintf(stderr,"RNPL: Unable to open <%s>\n",argv[optind]);
      exit(0);
    }
    rnplin=fpin;
    if(!oopt && (!strcmp(argv[optind]+strlen(argv[optind])-5,"_rnpl") || 
       !strcmp(argv[optind]+strlen(argv[optind])-5,".rnpl"))){
      strncpy(oname,argv[optind],strlen(argv[optind])-5);
      oname[strlen(argv[optind])-5]=0;
    }else if(!oopt) strcpy(oname,argv[2]);
    sprintf(iname,"%s_init",oname);
  }else if(oopt){
    sprintf(iname,"%s_init",oname);
  }else{
    ostd=1;
    strcpy(iname,"r_out_init");
    oname[0]=0;
  }

  if(language!=ALLF){
    strcat(iname,".c");
    strcat(oname,".c");
  }else{
    strcat(iname,".f");
    strcat(oname,".f");
  }
  
#ifdef RNPLDEBUG
fprintf(stderr,"iname=%s\n",iname);
fprintf(stderr,"oname=%s\n",oname);
#endif


  str_tbl.next=NULL;
  str_tbl.n=NULL;

  add_name("stub");
  add_name("auto");
  /* add "special" parameters */
  add_name("epsiter");
  add_name("epsiterid");
  add_name("maxstep");
  add_name("maxstepid");
  add_name("start_t");
  add_name("iter");
  add_name("fout");
  add_name("ser");
  add_name("output");
  add_name("trace");
  add_name("lambda");
  add_name("in_file");
  add_name("out_file");
  add_name("level");
  add_name("tag");
  add_name("s_step");
  add_name("out_gf");
  if(language == ALLF)
    add_name("memsiz");

  /* add functions to string table */
  Ln=add_name("log");
  Sin=add_name("sin");
  Cos=add_name("cos");
  Exp=add_name("exp");
  Tan=add_name("tan");
  Sqrt=add_name("sqrt");
  Sinh=add_name("sinh");
  Cosh=add_name("cosh");
  Tanh=add_name("tanh");
  ASin=add_name("asin");
  ACos=add_name("acos");
  ATan=add_name("atan");
  Abs=add_name("abs");

  /* setup declaration pointers */
  pd=NULL;
  cd=NULL;
  gd=NULL;
  gf=NULL;
  ad=NULL;
  od=NULL;
  rd=NULL;
  id=NULL;
  ld=NULL;
  ud=NULL;
  iz=NULL;

#ifdef RNPLDEBUG
fprintf(stderr,"Before rnplparse\n");
#endif

  rnplparse();

#ifdef RNPLDEBUG
fprintf(stderr,"After rnplparse\n");
#endif

  if(!ostd){
    fclose(fpin);
    fpout=fopen(oname,"w");
    if(fpout==NULL){
      fprintf(stderr,"Unable to open <%s> for output.\n",oname);
      exit(0);
    }
  }else fpout=stdout;
  fpout_init=fopen(iname,"w");
  if(fpout_init==NULL){
    fprintf(stderr,"Unable to open <%s> for output.\n",iname);
    exit(0);
  }

	oname[strlen(oname)-2]=0;
	iname[strlen(iname)-2]=0;
	
  setup_tables();
  
  /* error checking */
  /* check operators for: */
  /*  d_ops must have all coordinates in same system */
  /*  gfunc in d_op (f) must have correct rank in op_expr */
  /* check residuals for: */
  /*   each gfunc in a d_op must have the correct coordinates */
  
  /* check for undeclared names */
  /*
  for(nlp=str_tbl.next;nlp!=NULL;nlp=nlp->next)
    if(name_type(nlp)==-1)
      fprintf(stderr,"Error: <%s> is undeclared.\n",nlp->n);
  */
  for(i=0;i<nopers;i++){
#ifdef RNPLDEBUG
fprintf(stderr,"dopers[%d].op_expr=",i);
ex_to_string(stderr,dopers[i].op_expr);
fprintf(stderr,"\n");
#endif
    expand_expr(dopers[i].op_expr);
#ifdef RNPLDEBUG
fprintf(stderr,"expanded dopers[%d].op_expr=",i);
ex_to_string(stderr,dopers[i].op_expr);
fprintf(stderr,"\n");
#endif
    evaluate_expr(dopers[i].op_expr);
#ifdef RNPLDEBUG
fprintf(stderr,"evaluated dopers[%d].op_expr=",i);
ex_to_string(stderr,dopers[i].op_expr);
fprintf(stderr,"\n");
#endif
  }
  
  /* substitute operators into residuals */
  for(rn=0;rn<nresids;rn++){
    if(resids[rn].etype==1){
      expand_expr(resids[rn].elst.expr);
      evaluate_expr(resids[rn].elst.expr);
      offset_expr(resids[rn].toff,resids[rn].indx,resids[rn].elst.expr);
      simplify_expr(resids[rn].elst.expr); 
    }else{
      expand_ifstat(resids[rn].elst.ifst);
      evaluate_ifstat(resids[rn].elst.ifst);
      offset_ifstat(resids[rn].toff,resids[rn].indx,resids[rn].elst.ifst);
      simplify_ifstat(resids[rn].elst.ifst);
    }
  }
  

#ifdef RNPLDEBUG
fprintf(stderr,"substituted operators into residuals\n");
#endif
  
  /* check residuals for undeclared names, mascarading gfuncs and coords */
  for(rn=0;rn<nresids;rn++){
    for(ir=resids[rn].reg;ir!=NULL;ir=ir->next){
      if(!check_expr(ir->lower) || !check_simp_expr(ir->lower) ||
         !check_expr(ir->upper) || !check_simp_expr(ir->upper)){
        fprintf(stderr,"Error: residual %d for %s has a bad index expression\n",rn,
                resids[rn].gfunc->fname->n);
        fatal_error("Expression specification.");
      }
    }
    if(resids[rn].etype==1){
      if(!check_expr(resids[rn].elst.expr) || !check_gen_expr(resids[rn].elst.expr)){
        fprintf(stderr,"Error: residual %d for %s has an expression error\n",rn,
                resids[rn].gfunc->fname->n);
        fatal_error("Expression specification.");
      }
    }else{
      if(!check_ifstat(resids[rn].elst.ifst) || !check_gen_ifstat(resids[rn].elst.ifst)){
        fprintf(stderr,"Error: residual %d for %s\n",rn,resids[rn].gfunc->fname->n);
        fatal_error("Expression specification.");
      }
    }
  }
  
  /* check residuals for time levels */
  /* check residuals for ranges */
  
#ifdef RNPLDEBUG
fprintf(stderr,"residual expressions\n");
#endif

  /* substitutions into initializations */
  for(i=0;i<ninits;i++){
    if(inits[i].etype==1){
      expand_expr(inits[i].elst.expr);
      evaluate_expr(inits[i].elst.expr);
      simplify_expr(inits[i].elst.expr); 
    }else{
      expand_ifstat(inits[i].elst.ifst);
      evaluate_ifstat(inits[i].elst.ifst);
      simplify_ifstat(inits[i].elst.ifst);
    }
  }

#ifdef RNPLDEBUG
fprintf(stderr,"initialization expressions\n");
#endif
  /* check initializations for undeclared names */

  for(i=0;i<ninits;i++){
    for(ir=inits[i].reg;ir!=NULL;ir=ir->next){
      if(!check_expr(ir->lower) || !check_simp_expr(ir->lower) ||
         !check_expr(ir->upper) || !check_simp_expr(ir->upper)){
        fprintf(stderr,"Error: initialization %d for %s has a bad index expression\n",i,
                inits[i].gfunc->fname->n);
        fatal_error("Expression specification.");
      }
    }
    if(inits[i].etype==1){
      if(!check_expr(inits[i].elst.expr) || !check_gen_expr(inits[i].elst.expr)){
        fprintf(stderr,"Error: initialization %d for %s has an expression error\n",i,
                inits[i].gfunc->fname->n);
        fatal_error("Expression specification.");
      }
    }else{
      if(!check_ifstat(inits[i].elst.ifst) || !check_gen_ifstat(inits[i].elst.ifst)){
        fprintf(stderr,"Error: initialization %d for %s\n",i,inits[i].gfunc->fname->n);
        fatal_error("Expression specification.");
      }
    }
  }

  /* build reference lists */
  /* first resid or init for any gfunc contains complete reference lists */
  /* subsequent resids or inits contain only local ref lists */
  /* updates and initializers contain complete gfunc reference lists */

  for(rn=0;rn<nresids;rn++)
    resids[rn].output=0;
    
  /* do residuals */  
  for(rn=0;rn<nresids;rn++){
    name_list *nm;
    gfunc_ref_list *grl,*tgl;
    coord_list *gcl,*tcl;
    coord_list *gcd,*tcd;
    param_ref_list *prl,*tpl;
    if(!resids[rn].output){
      resids[rn].output=1;
      nm=resids[rn].gfunc->fname;
      if(resids[rn].etype==1){
        grl=find_gfuncs(resids[rn].elst.expr);
        gcl=find_coords(resids[rn].elst.expr);
        gcd=find_cdifs(resids[rn].elst.expr);
        prl=find_params(resids[rn].elst.expr);
      }else{
        grl=find_gfuncs_if(resids[rn].elst.ifst);
        gcl=find_coords_if(resids[rn].elst.ifst);
        gcd=find_cdifs_if(resids[rn].elst.ifst);
        prl=find_params_if(resids[rn].elst.ifst);
      }
      for(ir=resids[rn].reg;ir!=NULL;ir=ir->next){
        tcd=find_cdifs(ir->lower);
        if(gcd)
          add_name_ref(gcd,tcd);
        else gcd=tcd;
        tcd=find_cdifs(ir->upper);
        if(gcd)
          add_name_ref(gcd,tcd);
        else gcd=tcd;
        tpl=find_params(ir->lower);
        if(prl)
          add_param_ref(prl,tpl);
        else prl=tpl;
        tpl=find_params(ir->upper);
        if(prl)
          add_param_ref(prl,tpl);
        else prl=tpl;
      }
      resids[rn].gf_refs=dup_gf_ref(grl);
      resids[rn].crds=dup_clst(gcl);
      resids[rn].cdifs=dup_clst(gcl);
      resids[rn].parms=dup_param_ref(prl);
      for(i=rn+1;i<nresids && resids[i].gfunc->fname==nm;i++){
        resids[i].output=1;
        if(resids[i].etype==1){
          resids[i].gf_refs=find_gfuncs(resids[i].elst.expr);
          resids[i].crds=find_coords(resids[i].elst.expr);
          resids[i].cdifs=find_cdifs(resids[i].elst.expr);
          resids[i].parms=find_params(resids[i].elst.expr);
        }else{
          resids[i].gf_refs=find_gfuncs_if(resids[i].elst.ifst);
          resids[i].crds=find_coords_if(resids[i].elst.ifst);
          resids[i].cdifs=find_cdifs_if(resids[i].elst.ifst);
          resids[i].parms=find_params_if(resids[i].elst.ifst);
        }
        for(ir=resids[i].reg;ir!=NULL;ir=ir->next){
          tcd=find_cdifs(ir->lower);
          if(resids[i].cdifs)
            add_name_ref(resids[i].cdifs,tcd);
          else resids[i].cdifs=tcd;
          tcd=find_cdifs(ir->upper);
          if(resids[i].cdifs)
            add_name_ref(resids[i].cdifs,tcd);
          else resids[i].cdifs=tcd;
          tpl=find_params(ir->lower);
          if(resids[i].parms)
            add_param_ref(resids[i].parms,tpl);
          else resids[i].parms=tpl;
          tpl=find_params(ir->upper);
          if(resids[i].parms)
            add_param_ref(resids[i].parms,tpl);
          else resids[i].parms=tpl;
        }
        resids[i].glob_gf=NULL;
        resids[i].glob_crds=NULL;
        resids[i].glob_cdifs=NULL;
        resids[i].glob_par=NULL;
        tgl=dup_gf_ref(resids[i].gf_refs);
        if(grl){
          add_gfunc_ref(grl,tgl);
        }else grl=tgl;
        tcl=dup_clst(resids[i].crds);
        if(gcl){
          add_name_ref(gcl,tcl);
        }else gcl=tcl;
        tcd=dup_clst(resids[i].cdifs);
        if(gcd){
          add_name_ref(gcd,tcd);
        }else gcd=tcd;
        tpl=dup_param_ref(resids[i].parms);
        if(prl){
          add_param_ref(prl,tpl);
        }else prl=tpl;
      }
      resids[rn].glob_gf=grl;
      resids[rn].glob_crds=gcl;
      resids[rn].glob_cdifs=gcd;
      resids[rn].glob_par=prl;
    }
  }

#ifdef RNPLDEBUG
fprintf(stderr,"residual refs\n");
#endif

  for(rn=0;rn<ninits;rn++)
    inits[rn].output=0;
    
  /* do initializations */  
  for(rn=0;rn<ninits;rn++){
    name_list *nm;
    gfunc_ref_list *grl,*tgl,*gfr;
    coord_list *gcl,*tcl;
    coord_list *gcd,*tcd;
    param_ref_list *prl,*tpl;
    offset_type *o;
    int j;
    if(!inits[rn].output){
      inits[rn].output=1;
      nm=inits[rn].gfunc->fname;
      gfr=new_gfunc_ref();
      gfr->gfunc=inits[rn].gfunc;
      /*
      for(j=0,o=gfr->gfunc->tlev;j<gfr->gfunc->ntlevs-1;o=o->next,j++);
      gfr->toff=o->offset;
      */
      gfr->toff=inits[rn].toff;
      if(inits[rn].etype==1){
        grl=find_gfuncs(inits[rn].elst.expr);
        gcl=find_coords(inits[rn].elst.expr);
        gcd=find_cdifs(inits[rn].elst.expr);
        prl=find_params(inits[rn].elst.expr);
      }else{
        grl=find_gfuncs_if(inits[rn].elst.ifst);
        gcl=find_coords_if(inits[rn].elst.ifst);
        gcd=find_cdifs_if(inits[rn].elst.ifst);
        prl=find_params_if(inits[rn].elst.ifst);
      }
      if(grl)
        add_gfunc_ref(grl,gfr);
      else grl=gfr;
      for(ir=inits[rn].reg;ir!=NULL;ir=ir->next){
        tcd=find_cdifs(ir->lower);
        if(gcd)
          add_name_ref(gcd,tcd);
        else gcd=tcd;
        tcd=find_cdifs(ir->upper);
        if(gcd)
          add_name_ref(gcd,tcd);
        else gcd=tcd;
        tpl=find_params(ir->lower);
        if(prl)
          add_param_ref(prl,tpl);
        else prl=tpl;
        tpl=find_params(ir->upper);
        if(prl)
          add_param_ref(prl,tpl);
        else prl=tpl;
      }
      inits[rn].gf_refs=dup_gf_ref(grl);
      inits[rn].crds=dup_clst(gcl);
      inits[rn].cdifs=dup_clst(gcl);
      inits[rn].parms=dup_param_ref(prl);
      for(i=rn+1;i<ninits;i++){
      	if(inits[i].gfunc->fname==nm && !inits[i].output){
	        inits[i].output=1;
	        if(inits[i].etype==1){
	          inits[i].gf_refs=find_gfuncs(inits[i].elst.expr);
	          inits[i].crds=find_coords(inits[i].elst.expr);
	          inits[i].cdifs=find_cdifs(inits[i].elst.expr);
	          inits[i].parms=find_params(inits[i].elst.expr);
	        }else{
	          inits[i].gf_refs=find_gfuncs_if(inits[i].elst.ifst);
	          inits[i].crds=find_coords_if(inits[i].elst.ifst);
	          inits[i].cdifs=find_cdifs_if(inits[i].elst.ifst);
	          inits[i].parms=find_params_if(inits[i].elst.ifst);
	        }
	        tgl=new_gfunc_ref();
	        tgl->gfunc=inits[i].gfunc;
		      tgl->toff=inits[i].toff;
	        if(inits[i].gf_refs){
	        	add_gfunc_ref(inits[i].gf_refs,tgl);
	        }else inits[i].gf_refs=tgl;
	        for(ir=inits[i].reg;ir!=NULL;ir=ir->next){
	          tcd=find_cdifs(ir->lower);
	          if(inits[i].cdifs)
	            add_name_ref(inits[i].cdifs,tcd);
	          else inits[i].cdifs=tcd;
	          tcd=find_cdifs(ir->upper);
	          if(inits[i].cdifs)
	            add_name_ref(inits[i].cdifs,tcd);
	          else inits[i].cdifs=tcd;
	          tpl=find_params(ir->lower);
	          if(inits[i].parms)
	            add_param_ref(inits[i].parms,tpl);
	          else inits[i].parms=tpl;
	          tpl=find_params(ir->upper);
	          if(inits[i].parms)
	            add_param_ref(inits[i].parms,tpl);
	          else inits[i].parms=tpl;
	        }
	        inits[i].glob_gf=NULL;
	        inits[i].glob_crds=NULL;
	        inits[i].glob_cdifs=NULL;
	        inits[i].glob_par=NULL;
	        tgl=dup_gf_ref(inits[i].gf_refs);
	        if(grl){
	          add_gfunc_ref(grl,tgl);
	        }else grl=tgl;
	        tcl=dup_clst(inits[i].crds);
	        if(gcl){
	          add_name_ref(gcl,tcl);
	        }else gcl=tcl;
	        tcd=dup_clst(inits[i].cdifs);
	        if(gcd){
	          add_name_ref(gcd,tcd);
	        }else gcd=tcd;
	        tpl=dup_param_ref(inits[i].parms);
	        if(prl){
	          add_param_ref(prl,tpl);
	        }else prl=tpl;
        }
      }
      inits[rn].glob_gf=grl;
      inits[rn].glob_crds=gcl;
      inits[rn].glob_cdifs=gcd;
      inits[rn].glob_par=prl;
    }
  }

#ifdef RNPLDEBUG
fprintf(stderr,"initilization refs\n");
#endif

  /* do updates */
  for(i=0;i<nupdates;i++){ /* loop over updates */
    gfunc_tab_list *gtl;
    gfunc_ref_list *tgl,*gl;
    coord_list *tcl,*cl;
    param_ref_list *tpl,*pl;
    work_list *wl;
    if(!updates[i].header){
    	 /* take care of expressions in work arrays */
    	for(wl=updates[i].work_refs;wl!=NULL;wl=wl->next){
        gl=find_gfuncs(wl->work->expr);
        if(updates[i].glob_gf){ /* if list not empty, add new items */
          add_gfunc_ref(updates[i].glob_gf,gl);
        }else updates[i].glob_gf=gl; /* copy */
        cl=find_coords(wl->work->expr);
        if(updates[i].glob_crds){ /* if list not empty, add new items */
          add_name_ref(updates[i].glob_crds,cl);
        }else updates[i].glob_crds=cl; /* copy */
        cl=find_cdifs(wl->work->expr);
        if(updates[i].glob_cdifs){ /* if list not empty, add new items */
          add_name_ref(updates[i].glob_cdifs,cl);
        }else updates[i].glob_cdifs=cl; /* copy */
        pl=find_params(wl->work->expr);
        if(updates[i].glob_par){ /* if list is not empty, add new items */
          add_param_ref(updates[i].glob_par,pl);
        }else updates[i].glob_par=pl;
      }
      for(gtl=updates[i].gfs;gtl!=NULL;gtl=gtl->next){ /* loop over gfuncs */
        rn=resid_exists(gtl->gfunc->fname);
        if(rn==-1){
          fprintf(stderr,"Update %d ",i);
          if(updates[i].name)
            fprintf(stderr,"(%s) ",updates[i].name->n);
          fprintf(stderr,"defined for %s has no header or residual.\n",
                  gtl->gfunc->fname->n);
          fatal_error("Specification error.");
        }
        tgl=dup_gf_ref(resids[rn].glob_gf);
        if(updates[i].glob_gf){ /* if list not empty, add new items */
          add_gfunc_ref(updates[i].glob_gf,tgl);
        }else updates[i].glob_gf=tgl; /* copy */
        tcl=dup_clst(resids[rn].glob_crds);
        if(updates[i].glob_crds){ /* if list not empty, add new items */
          add_name_ref(updates[i].glob_crds,tcl);
        }else updates[i].glob_crds=tcl; /* copy */
        tcl=dup_clst(resids[rn].glob_cdifs);
        if(updates[i].glob_cdifs){ /* if list not empty, add new items */
          add_name_ref(updates[i].glob_cdifs,tcl);
        }else updates[i].glob_cdifs=tcl; /* copy */
        tpl=dup_param_ref(resids[rn].glob_par);
        if(updates[i].glob_par){ /* if list is not empty, add new items */
          add_param_ref(updates[i].glob_par,tpl);
        }else updates[i].glob_par=tpl;
      }
    }
  }
  
#ifdef RNPLDEBUG
fprintf(stderr,"update refs\n");
#endif

  /* do initializers */
  for(i=0;i<niniters;i++){ /* loop over initializers */
    gfunc_tab_list *gtl;
    gfunc_ref_list *tgl,*gl;
    coord_list *tcl,*cl;
    param_ref_list *tpl,*pl;
    work_list *wl;
    if(!initers[i].header){
    	 /* take care of expressions in work arrays */
    	for(wl=initers[i].work_refs;wl!=NULL;wl=wl->next){
        gl=find_gfuncs(wl->work->expr);
        if(initers[i].glob_gf){ /* if list not empty, add new items */
          add_gfunc_ref(initers[i].glob_gf,gl);
        }else initers[i].glob_gf=gl; /* copy */
        cl=find_coords(wl->work->expr);
        if(initers[i].glob_crds){ /* if list not empty, add new items */
          add_name_ref(initers[i].glob_crds,cl);
        }else initers[i].glob_crds=cl; /* copy */
        cl=find_cdifs(wl->work->expr);
        if(initers[i].glob_cdifs){ /* if list not empty, add new items */
          add_name_ref(initers[i].glob_cdifs,cl);
        }else initers[i].glob_cdifs=cl; /* copy */
        pl=find_params(wl->work->expr);
        if(initers[i].glob_par){ /* if list is not empty, add new items */
          add_param_ref(initers[i].glob_par,pl);
        }else initers[i].glob_par=pl;
      }
      for(gtl=initers[i].gfs;gtl!=NULL;gtl=gtl->next){ /* loop over gfuncs */
        rn=init_exists(gtl->gfunc->fname);
        if(rn==-1){
          fprintf(stderr,"Initializer %d ",i);
          if(initers[i].name)
            fprintf(stderr,"(%s) ",initers[i].name->n);
          fprintf(stderr,"defined for %s has no header or initialization.\n",
                  gtl->gfunc->fname->n);
          fatal_error("Specification error.");
        }
        tgl=dup_gf_ref(inits[rn].glob_gf);
        if(initers[i].glob_gf){ /* if list not empty, add new items */
          add_gfunc_ref(initers[i].glob_gf,tgl);
        }else initers[i].glob_gf=tgl; /* copy */
        tcl=dup_clst(inits[rn].glob_crds);
        if(initers[i].glob_crds){ /* if list not empty, add new items */
          add_name_ref(initers[i].glob_crds,tcl);
        }else initers[i].glob_crds=tcl; /* copy */
        tcl=dup_clst(inits[rn].glob_cdifs);
        if(initers[i].glob_cdifs){ /* if list not empty, add new items */
          add_name_ref(initers[i].glob_cdifs,tcl);
        }else initers[i].glob_cdifs=tcl; /* copy */
        tpl=dup_param_ref(inits[rn].glob_par);
        if(initers[i].glob_par){ /* if list is not empty, add new items */
          add_param_ref(initers[i].glob_par,tpl);
        }else initers[i].glob_par=tpl;
      }
    }
  }
  
#ifdef RNPLDEBUG
fprintf(stderr,"initializer refs\n");
#endif

  /* set up list of static work arrays */
  static_work=NULL;
  for(i=0;i<nupdates;i++){ /* work arrays from updates */
    work_list *wl,*wn;
    work_array *wk;
    for(wl=updates[i].work_refs;wl!=NULL;wl=wl->next){
      if(!wl->work->ato){ /* if static, dup and add to list */
        wk=new_work_array();
        wk->ato=0;
        wk->num=wl->work->num;
        wk->expr=dup_expr(wl->work->expr);
        wn=new_work_list();
        wn->work=wk;
        if(static_work)
          add_work_ref(static_work,wn);
        else static_work=wn;
      }
    }
  }
  for(i=0;i<niniters;i++){ /* work arrays from initializers */
    work_list *wl,*wn;
    work_array *wk;
    for(wl=initers[i].work_refs;wl!=NULL;wl=wl->next){
      if(!wl->work->ato){ /* if static, dup and add to list */
        wk=new_work_array();
        wk->ato=0;
        wk->num=wl->work->num;
        wk->expr=dup_expr(wl->work->expr);
        wn=new_work_list();
        wn->work=wk;
        if(static_work)
          add_work_ref(static_work,wn);
        else static_work=wn;
      }
    }
  }

#ifdef RNPLDEBUG
fprintf(stderr,"static work arrays\n");
#endif

  /* set up global lists of update refs */
  update_glob_gf=NULL;
  update_glob_par=NULL;
  update_glob_crds=NULL;
  update_glob_cdifs=NULL;
  for(i=0;i<nupdates;i++){
    gfunc_ref_list *tgl;
    coord_list *tcl;
    param_ref_list *tpl;
    work_list *twl;
    tgl=dup_gf_ref(updates[i].glob_gf);
    if(update_glob_gf){ /* if list not empty, add new items */
      add_gfunc_ref(update_glob_gf,tgl);
    }else update_glob_gf=tgl; /* copy */
    tcl=dup_clst(updates[i].glob_crds);
    if(update_glob_crds){ /* if list not empty, add new items */
      add_name_ref(update_glob_crds,tcl);
    }else update_glob_crds=tcl; /* copy */
    tcl=dup_clst(updates[i].glob_cdifs);
    if(update_glob_cdifs){ /* if list not empty, add new items */
      add_name_ref(update_glob_cdifs,tcl);
    }else update_glob_cdifs=tcl; /* copy */
    tpl=dup_param_ref(updates[i].glob_par);
    if(update_glob_par){ /* if list is not empty, add new items */
      add_param_ref(update_glob_par,tpl);
    }else update_glob_par=tpl;
    twl=dup_work_list(updates[i].work_refs);
    if(update_auto_work){
    	add_work_ref(update_auto_work,twl);
    }else update_auto_work=twl;
  }

	/* check for undefined initializations */
	/* and if we need to (and can) iterate the initial data */
	for(gn=0;gn<ngfuncs;gn++){
		int j=0;
		gfuncs[gn].neediditer=0;
		if(gfuncs[gn].ntlevs==3){ /* only know how to iterate 3-level schemes */
			i=initoff_exists(gfuncs[gn].fname,gfuncs[gn].tlev->next->offset);
			j=initoff_exists(gfuncs[gn].fname,gfuncs[gn].tlev->next->next->offset);
			if(i!=-1 && j==-1){ /* missing an explicit initilization */
				if(initer_exists(gfuncs[gn].fname)==-1){ /* no initializer either */
					neediditer=1;
					gfuncs[gn].neediditer=1;
				}
			}else if(i==-1 && j==-1){
				if(initer_exists(gfuncs[gn].fname)==-1){ /* error */
					fprintf(stderr,"No initializer exists for %s.\n",gfuncs[gn].fname->n);
					fatal_error("Specification error.");
				}
			}else if(i==-1 && j!=-1){
				if(initer_exists(gfuncs[gn].fname)==-1){
					fprintf(stderr,"No initializer ixists for %s",gfuncs[gn].fname->n);
					gfunc_suffix(stderr,gfuncs[gn].tlev->next->offset);
					fprintf(stderr,".\n");
					fatal_error("Specification error.");
				}
			}
		}
	}
	
  for(rn=0;rn<nresids;resids[rn++].output=0);
  for(rn=0;rn<ninits;inits[rn++].output=0);

  /* output evolution program */
  switch(language){
  	case IDF :
    case C   :  make_c_header(fpout);
                fpout_upd=fopen("updates.c","w");
                if(fpout_upd==NULL)
                  fatal_error("Can't open updates.c");
                fpout_res=fopen("residuals.c","w");
                if(fpout_res==NULL)
                	fatal_error("Can't open residuals.c");
 								make_c_header(fpout_res);
 								make_c_header(fpout_upd);
                break;
		case UPF :
    case F90 :  
    case F77 :  make_c_header(fpout);
                fpout_upd=fopen("updates.f","w");
                if(fpout_upd==NULL)
                  fatal_error("Can't open updates.f");
                fpout_res=fopen("residuals.f","w");
                if(fpout_res==NULL)
                	fatal_error("Can't open residuals.f");
                break;
    case ALLF:  make_f77_header(fpout);
                fpout_res=fopen("residuals.f","w");
                if(fpout_res==NULL)
                	fatal_error("Can't open residuals.f");
    						fpout_inc=fopen("globals.inc","w");
    						if(fpout_inc==NULL)
    							fatal_error("Can't open globals.inc");
    						fpout_inc1=fopen("other_glbs.inc","w");
    						if(fpout_inc1==NULL)
    							fatal_error("Can't open other_glbs.inc");
                fpout_upd=fopen("updates.f","w");
                if(fpout_upd==NULL)
                  fatal_error("Can't open updates.f");
							  declare_coord_difs(fpout_inc1); 
							  declare_parameters(fpout_inc);
							  declare_grids(fpout_inc); 
							  declare_gfuncs(fpout_inc);
							  declare_attributes(fpout_inc); 
							  init_coord_difs(fpout); 
							  init_grids(fpout); 
							  init_gfuncs(fpout);
                break;
  }

  code_residuals(fpout_res); 
  swap_levels(fpout); 
  one_step(fpout_upd); 
  code_updates(fpout); 
  init_params_attribs(fpout);
  read_params_attribs(fpout);
  read_state(fpout,iname);
  write_params_attribs(fpout);
  dump_state(fpout,1);
  handler(fpout);
  output_func(fpout);
  
  switch(language){
  	case UPF :
  	case IDF :
  	case F77 :
  	case F90 :
  	case C   :	make_c_main(fpout,oname);
  							break;
  	case ALLF:	cleanup(fpout);
								make_f77_main(fpout,oname);
  							break;
  }

  /* output initial data generator */

  switch(language){
  	case UPF :
    case C   :  make_c_header(fpout_init);
                fpout_initer=fopen("initializers.c","w");
                if(fpout_initer==NULL)
                  fatal_error("Can't open initializers.c");
 								make_c_header(fpout_initer);
                break;
    case IDF :
    case F77 : 
    case F90 :  make_c_header(fpout_init);
                fpout_initer=fopen("initializers.f","w");
                if(fpout_initer==NULL)
                  fatal_error("Can't open initializers.f");
                break;
    case ALLF:  make_f77_header(fpout_init);
                fpout_initer=fopen("initializers.f","w");
                if(fpout_initer==NULL)
                  fatal_error("Can't open initializers.f");
							  init_coord_difs(fpout_init);
							  init_grids(fpout_init);
							  init_gfuncs(fpout_init);
                break;
  }

  if(neediditer){
	  swap_top(fpout_init);
	  code_iteration(fpout_init);
	}
  init_params_attribs(fpout_init);
  gen_gfuncs(fpout_initer);
  write_params_attribs(fpout_init);
  dump_state(fpout_init,0);

  switch(language){
  	case IDF :
  	case UPF :
    case F77 : 
    case F90 :
    case C   :  make_c_init_main(fpout_init,iname);
                break;
    case ALLF:  cleanup(fpout_init);
    						make_f77_init_main(fpout_init,iname);
                break;
  }
  if(language==ALLF){
	  fclose(fpout_inc);
	  fclose(fpout_inc1);
	}
  fclose(fpout_init);
  fclose(fpout);

  if(language==ALLF){
    output_gfuni0();
  }

	/* generate attributes file */
  if((fpout=fopen(".rnpl.attributes","w"))==NULL)
    fatal_error("Can't open .rnpl.attributes.");
  else {
    offset_type *o;
    fprintf(fpout,"%d\n",nattribs);
    for(i=0;i<nattribs;i++){
      fprintf(fpout,"%s\n",attribs[i].name->n);
      fprintf(fpout,"%d\n",attribs[i].size);
      if(attribs[i].encoding==ENCODEONE){
        for(fn=0;fn<ngfuncs;fn++)
          fprintf(fpout,"%s ",gfuncs[fn].fname->n);
      }else { /* ENCODEALL */
        for(fn=0;fn<ngfuncs;fn++)
          for(o=gfuncs[fn].tlev;o!=NULL;o=o->next){
            fprintf(fpout,gfuncs[fn].fname->n);
            gfunc_suffix(fpout,o->offset);
            fprintf(fpout," ");
          }
      }
      for(rn=0;rn<nresids;){
        if(resids[rn].eval)
          fprintf(fpout,"%s_res ",resids[rn].gfunc->fname->n);
        for(++rn;rn<nresids && resids[rn].gfunc==resids[rn-1].gfunc;rn++);
      }
      fprintf(fpout,"\n%s := ",attribs[i].name->n);
      if(attribs[i].size>1)
        fprintf(fpout,"[ ");
      if(attribs[i].def){
        for(fn=0;fn<attribs[i].size;fn++)
          switch(attribs[i].type){
            case INT    : fprintf(fpout,"%d ",attribs[i].def_val.i_ar[fn]);
                          break;
            case FLOAT  :  fprintf(fpout,"%.16g ",attribs[i].def_val.f_ar[fn]);
                          break;
            case STRING  : if(attribs[i].def_val.s_ar[rn]!=NULL)
                            fprintf(fpout,"\"%s\" ",attribs[i].def_val.s_ar[fn]);
                          else fprintf(fpout,"\" \" ");
                          break;
          }
      }else{
        for(fn=0;fn<attribs[i].size;fn++)
          switch(attribs[i].type){
            case INT    :
            case FLOAT  : fprintf(fpout,"0 ");
                          break;
            case STRING  : fprintf(fpout,"\" \" ");
                          break;
          }
      }
      if(attribs[i].size>1)
        fprintf(fpout,"]\n");      
    }
    fclose(fpout);
  }
}

void setup_tables()
{
  int an,gn,fn,dn,rn,i,pn;
  param_dec *pl,*np;
  grid_list *g;
  gfunc_list *f;
  attrib_dec *al,*na;
  init_dec *ind;
  operator *o;
  residual *r;
  res_list *rl;
  update *u;
  i_reg *p;
  c_reg *q;
  char *fnm;
  coord_list *co,*nm;
  gfunc_tab_list *gft;
  coord_dec *crd;


#ifdef RNPLDEBUG
fprintf(stderr,"Setting up tables\n");
#endif

  /* setup coordinates */
  for(gn=0,crd=cd;crd!=NULL;crd=crd->next,gn++);
  if(cd==NULL)
    fatal_error("No coordinate system.");
  if((coords=(coord_table *)malloc(gn*sizeof(coord_table)))==NULL)
    fatal_error("Can't malloc coord_table.");
  ncoords=gn;
  for(--gn,crd=cd;gn>=0;gn--,crd=crd->next){ /* reverse order */
    coords[gn].rank=crd->rank;
    coords[gn].name=crd->name;
    coords[gn].c_names=crd->c_names;
  }
       
  if((coord_difs=(name_list ***)malloc(ncoords*sizeof(name_list **)))==NULL)
    fatal_error("Can't malloc coord_difs.");
  for(gn=0;gn<ncoords;gn++)
    if((coord_difs[gn]=(name_list **)malloc(coords[gn].rank*sizeof(name_list *)))==NULL)
      fatal_error("Can't malloc coord_difs num.");
 
  if((grid_base=(name_list ***)malloc(ncoords*sizeof(name_list **)))==NULL)
    fatal_error("Can't malloc grid_base.");
  for(gn=0;gn<ncoords;gn++)
    if((grid_base[gn]=(name_list **)malloc((coords[gn].rank-1)*sizeof(name_list *)))==NULL)
      fatal_error("Can't malloc grid_base num.");
     

#ifdef RNPLDEBUG
fprintf(stderr,"setup coordinates\n");
fprintf(stderr,"ncoords=%d\n",ncoords);
for(i=0;i<ncoords;i++)
  fprintf(stderr,"coords[%d].name=%s\n",i,coords[i].name->n);
#endif

  /* error checking */
  /* make sure there are no duplicates amoung spacial coordinates */
  for(co=coords[0].c_names->next,i=1;i<coords[0].rank;i++,co=co->next)
   for(nm=coords[0].c_names,gn=0;gn<i;gn++,nm=nm->next)
     if(nm->name==co->name){
       fprintf(stderr,"Duplicate coordinate name %s in 1st coordinate system.\n",
                nm->name->n);
       fatal_error("Specification error\n");
     }
  for(i=1;i<ncoords;i++)
   for(gn=0;gn<i;gn++)
     for(nm=coords[i].c_names->next,rn=1;rn<coords[i].rank;rn++,nm=nm->next)
       if(in_clst(nm->name,&coords[gn])){
         fprintf(stderr,"Duplicate coordinate name %s between coordinates systems %d and %d.\n",
                  nm->name->n,i,gn);
         fatal_error("Specification error\n");
       }
  
#ifdef RNPLDEBUG
fprintf(stderr,"Checked coordinates\n");
#endif

  /* add coord_difs and grid bases*/
  for(fn=0;fn<ncoords;fn++)
    for(i=0,co=coords[fn].c_names;i<coords[fn].rank;i++,co=co->next){
      fnm=new_string(strlen(co->name->n)+1);
      sprintf(fnm,"d%s",co->name->n);
      coord_difs[fn][i]=add_name(fnm);
      free(fnm);
    }
  for(fn=0;fn<ncoords;fn++)
    for(i=0,co=coords[fn].c_names->next;i<coords[fn].rank-1;i++,co=co->next){
      fnm=new_string(strlen(co->name->n)+1);
      sprintf(fnm,"N%s",co->name->n);
      grid_base[fn][i]=add_name(fnm);
      free(fnm);
    }

#ifdef RNPLDEBUG
fprintf(stderr,"Added cdifs and gbases\n");
#endif

  /* build table of parameters */
  pl=pd;
  np=new_param_dec();
  np->type=STRING;
  np->size=new_v_size();
  np->size->dim=1;
  np->size->size=new_i_ar(1);
  np->size->size[0]=1;
  np->name=name_lookup("in_file");
  np->def=0;
  np->con=1;
  np->def_val.s_ar=new_s_ar(1);
  if(pl)
    add_param(pl,np);
  else pl=pd=np;

  np=new_param_dec();
  np->type=STRING;
  np->size=new_v_size();
  np->size->dim=1;
  np->size->size=new_i_ar(1);
  np->size->size[0]=1;
  np->name=name_lookup("out_file");
  np->def=0;
  np->con=1;
  np->def_val.s_ar=new_s_ar(1);
  add_param(pl,np);

  np=new_param_dec();
  np->type=IVEC;
  np->size=new_v_size();
  np->size->dim=1;
  np->size->size=new_i_ar(1);
  np->size->size[0]=5;
  np->name=name_lookup("output");
  np->def=1;
  np->con=0;
  np->def_val.i_ar=new_i_ar(5);
  np->def_val.i_ar[0]=1;
  np->def_val.i_ar[1]=-1;
  np->def_val.i_ar[2]=-1;
  np->def_val.i_ar[3]=1;
  np->def_val.i_ar[4]=0;
  add_param(pl,np);
  
  np=new_param_dec();
  np->type=IVEC;
  np->size=new_v_size();
  np->size->dim=1;
  np->size->size=new_i_ar(1);
  np->size->size[0]=5;
  np->name=name_lookup("trace");
  np->def=1;
  np->con=0;
  np->def_val.i_ar=new_i_ar(5);
  np->def_val.i_ar[0]=1;
  np->def_val.i_ar[1]=-1;
  np->def_val.i_ar[2]=-1;
  np->def_val.i_ar[3]=1;
  np->def_val.i_ar[4]=0;
  add_param(pl,np);
  
  np=new_param_dec();
  np->type=INT;
  np->size=new_v_size();
  np->size->dim=1;
  np->size->size=new_i_ar(1);
  np->size->size[0]=1;
  np->name=name_lookup("ser");
  np->def=1;
  np->con=1;
  np->def_val.i_ar=new_i_ar(1);
  np->def_val.i_ar[0]=0;
  add_param(pl,np);

  np=new_param_dec();
  np->type=INT;
  np->size=new_v_size();
  np->size->dim=1;
  np->size->size=new_i_ar(1);
  np->size->size[0]=1;
  np->name=name_lookup("fout");
  np->def=1;
  np->con=1;
  np->def_val.i_ar=new_i_ar(1);
  np->def_val.i_ar[0]=0;
  add_param(pl,np);

  np=new_param_dec();
  np->type=INT;
  np->size=new_v_size();
  np->size->dim=1;
  np->size->size=new_i_ar(1);
  np->size->size[0]=1;
  np->name=name_lookup("iter");
  np->def=1;
  np->con=1;
  np->def_val.i_ar=new_i_ar(1);
  np->def_val.i_ar[0]=100;
  add_param(pl,np);

  np=new_param_dec();
  np->type=FLOAT;
  np->size=new_v_size();
  np->size->dim=1;
  np->size->size=new_i_ar(1);
  np->size->size[0]=1;
  np->name=name_lookup("start_t");
  np->def=1;
  np->con=1;
  np->def_val.f_ar=new_f_ar(1);
  np->def_val.f_ar[0]=0.0;
  add_param(pl,np);
 
  np=new_param_dec();
  np->type=FLOAT;
  np->size=new_v_size();
  np->size->dim=1;
  np->size->size=new_i_ar(1);
  np->size->size[0]=1;
  np->name=name_lookup("epsiter");
  np->def=1;
  np->con=0;
  np->def_val.f_ar=new_f_ar(1);
  np->def_val.f_ar[0]=1.0e-5;
  add_param(pl,np);
 
  np=new_param_dec();
  np->type=FLOAT;
  np->size=new_v_size();
  np->size->dim=1;
  np->size->size=new_i_ar(1);
  np->size->size[0]=1;
  np->name=name_lookup("epsiterid");
  np->def=1;
  np->con=0;
  np->def_val.f_ar=new_f_ar(1);
  np->def_val.f_ar[0]=1.0e-5;
  add_param(pl,np);
 
  np=new_param_dec();
  np->type=INT;
  np->size=new_v_size();
  np->size->dim=1;
  np->size->size=new_i_ar(1);
  np->size->size[0]=1;
  np->name=name_lookup("maxstep");
  np->def=1;
  np->con=1;
  np->def_val.i_ar=new_i_ar(1);
  np->def_val.i_ar[0]=50;
  add_param(pl,np);

  np=new_param_dec();
  np->type=INT;
  np->size=new_v_size();
  np->size->dim=1;
  np->size->size=new_i_ar(1);
  np->size->size[0]=1;
  np->name=name_lookup("maxstepid");
  np->def=1;
  np->con=1;
  np->def_val.i_ar=new_i_ar(1);
  np->def_val.i_ar[0]=50;
  add_param(pl,np);

  np=new_param_dec();
  np->type=FLOAT;
  np->size=new_v_size();
  np->size->dim=1;
  np->size->size=new_i_ar(1);
  np->size->size[0]=1;
  np->name=name_lookup("lambda");
  np->def=1;
  np->con=0;
  np->def_val.f_ar=new_f_ar(1);
  np->def_val.f_ar[0]=0.5;
  add_param(pl,np);

  np=new_param_dec();
  np->type=INT;
  np->size=new_v_size();
  np->size->dim=1;
  np->size->size=new_i_ar(1);
  np->size->size[0]=1;
  np->name=name_lookup("level");
  np->def=1;
  np->con=1;
  np->def_val.i_ar=new_i_ar(1);
  np->def_val.i_ar[0]=0;
  add_param(pl,np);

  np=new_param_dec();
  np->type=STRING;
  np->size=new_v_size();
  np->size->dim=1;
  np->size->size=new_i_ar(1);
  np->size->size[0]=1;
  np->name=name_lookup("tag");
  np->def=1;
  np->con=1;
  np->def_val.s_ar=new_s_ar(1);
  np->def_val.s_ar[0]=new_string(1);
  np->def_val.s_ar[0][0]='\0';
  add_param(pl,np);
 
  np=new_param_dec();
  np->type=INT;
  np->size=new_v_size();
  np->size->dim=1;
  np->size->size=new_i_ar(1);
  np->size->size[0]=1;
  np->name=name_lookup("s_step");
  np->def=1;
  np->con=1;
  np->def_val.i_ar=new_i_ar(1);
  np->def_val.i_ar[0]=0;
  add_param(pl,np);

  if(language==ALLF){
    np=new_param_dec();
    np->type=INT;
    np->size=new_v_size();
    np->size->dim=1;
    np->size->size=new_i_ar(1);
    np->size->size[0]=1;
    np->name=name_lookup("memsiz");
    np->def=1;
    np->con=2;
    np->def_val.i_ar=new_i_ar(1);
    np->def_val.i_ar[0]=2000000;
    add_param(pl,np);
  }
  
  /* add grid base parameters */
  for(gn=0;gn<ncoords;gn++)
    for(i=0,co=coords[gn].c_names->next;i<coords[gn].rank-1;i++,co=co->next){
      fnm=new_string(strlen(co->name->n)+2);
      sprintf(fnm,"N%s0",co->name->n);
      np=new_param_dec();
      np->type=INT;
      np->size=new_v_size();
      np->size->dim=1;
      np->size->size=new_i_ar(1);
      np->size->size[0]=1;
      np->con=1;
      np->name=add_name(fnm);
      free(fnm);
      np->def=1;
      np->def_val.i_ar=new_i_ar(1);
      np->def_val.i_ar[0]=3;
      add_param(pl,np);
    }

  for(pn=0;pl!=NULL;pl=pl->next,pn++);
  if((params=(param_table *)malloc((pn)*sizeof(param_table)))==NULL)
    fatal_error("Can't malloc param_table.");
  nparams=pn;
 
  for(pn=0,pl=pd;pl!=NULL;pl=pl->next,pn++){
    params[pn].type=pl->type;
    params[pn].size=pl->size;
    params[pn].name=pl->name;
    params[pn].con=pl->con;
    params[pn].def=pl->def;
    params[pn].def_val=pl->def_val;
  }


#ifdef RNPLDEBUG
fprintf(stderr,"setup parameters\n");
#endif

  /* build table of grids */
  for(gn=0,g=gd;g!=NULL;g=g->next,gn++);
  if(gn!=0){
    if((grids=(grid_table *)malloc(gn*sizeof(grid_table)))==NULL)
      fatal_error("Can't malloc grid_table.");
  }else grids=NULL;
  ngrids=gn;
  for(--gn,g=gd;gn>=0;gn--,g=g->next){ /* reverse order */
    int nc;
   

#ifdef RNPLDEBUG
fprintf(stderr,"grid %s\n",g->name->n);
#endif

    grids[gn].type=g->type;
    grids[gn].name=g->name;
    for(i=0;i<ncoords && coords[i].name != g->cname;i++);
    if(i==ncoords){
      fprintf(stderr,"Grid %s is defined on non-existant coordinate system %s.\n",g->name->n,
              g->cname->n);
      fatal_error("Specification error.");
    }else grids[gn].crds=&coords[i];

#ifdef RNPLDEBUG
fprintf(stderr,"grids[%d].crds->name->n=%s\n",gn,coords[i].name->n);
#endif

    if(g->clst != NULL){

#ifdef RNPLDEBUG
fprintf(stderr,"clst not null\n");
#endif

      for(nc=0,co=g->clst;co!=NULL;nc++,co=co->next); /* get number of coordinates */
      for(i=0,co=g->clst;i<nc;i++,co=co->next)
        if(!in_clst(co->name,grids[gn].crds) || !is_space(co->name)){
          fprintf(stderr,"Coordinate %s for grid %s is not a spacial coordinate for system %s\n",
                   co->name->n,grids[gn].name->n,grids[gn].crds->name->n);
          fatal_error("Bad grid declaration.");
        }
      grids[gn].clst=g->clst;
    }else{

#ifdef RNPLDEBUG
fprintf(stderr,"clst null\n");
#endif

      if(g->ireg==NULL){
        nc=grids[gn].crds->rank-1;
        grids[gn].clst=dup_clst(grids[gn].crds->c_names->next);
      }
    }
   if(g->ireg != NULL){ 

#ifdef RNPLDEBUG
fprintf(stderr,"ireg not null\n");
#endif

     for(i=0,p=g->ireg;p!=NULL;p=p->next,i++);
     grids[gn].reg.rank=i;

#ifdef RNPLDEBUG
fprintf(stderr,"grids[%d].reg.rank=%d\n",gn,i);
#endif

     if(g->clst==NULL){
       nc=i;
       grids[gn].clst=dup_clst(grids[gn].crds->c_names->next);
       for(i=1,co=grids[gn].clst;i<nc;i++,co=co->next);
       del_clst(co->next);
       co->next=NULL;
     }

#ifdef RNPLDEBUG
fprintf(stderr,"nc=%d\n",nc);
#endif

     if(grids[gn].reg.rank != nc){
       fprintf(stderr,"Grid %s has conflicting ranks %d and %d.\n",
               grids[gn].name->n,nc,grids[gn].reg.rank);
       fatal_error("Bad grid declaration.");
     }
     grids[gn].reg.bounds=g->ireg;
     for(i=0,q=g->creg;q!=NULL;q=q->next,i++);
     if(i!=grids[gn].reg.rank){
       fprintf(stderr,"Grid %s has rank %d for bounds and %d for limits.\n",
               grids[gn].name->n,grids[gn].reg.rank,i);
       fatal_error("Bad grid declaration.");
     }
     grids[gn].reg.limits=g->creg;
   }else{ /* use defaults */
     i_reg *ir;
     c_reg *cr;
     node *n;
     int i;
     

#ifdef RNPLDEBUG
fprintf(stderr,"ireg null\n");
#endif

     grids[gn].reg.rank=nc;
     ir=new_i_reg();
     cr=new_c_reg();
     grids[gn].reg.bounds=ir;
     grids[gn].reg.limits=cr;
     for(i=0,nm=grids[gn].clst;i<nc;i++,nm=nm->next){
       fnm=new_string(strlen(nm->name->n)+3);
       sprintf(fnm,"%smin",nm->name->n);
       cr->lower=name_lookup(fnm);
       if(cr->lower==NULL){
         fprintf(stderr,"Failed to declare %s\n",fnm);
         fatal_error("Missing parameter declaration.");
       }
       sprintf(fnm,"%smax",nm->name->n);
       cr->upper=name_lookup(fnm);
       if(cr->upper==NULL){
         fprintf(stderr,"Failed to declare %s\n",fnm);
         fatal_error("Missing parameter declaration.");
       }
       n=new_node();
       n->type=INUM;
       if(language==C)
         n->data.inum=0;
       else n->data.inum=1;
       ir->lower=n;
       sprintf(fnm,"N%s",nm->name->n);
       add_name(fnm);
       n=new_node();
       if(language==C){ /* Nc-1 */
         n->type=OP;
         n->data.op='-';
         n->left=new_node();
         n->left->type=IDEN;
         n->left->data.name=name_lookup(fnm);
         n->right=new_node();
         n->right->type=INUM;
         n->right->data.inum=1;
       }else{ /* Nc */
         n->type=IDEN;
         n->data.name=name_lookup(fnm);
       }
       ir->upper=n;
       if(i+1 < nc){
         ir->next=new_i_reg();
         cr->next=new_c_reg();
         ir=ir->next;
         cr=cr->next;
       }
       free(fnm);
     }
   }
 }


#ifdef RNPLDEBUG
fprintf(stderr,"setup grids\n");
#endif

  /* build table of gfunctions */
  for(fn=0,f=gf;f!=NULL;f=f->next,fn++);
  if(fn!=0){
    if((gfuncs=(gfunc_table *)malloc(fn*sizeof(gfunc_table)))==NULL)
      fatal_error("Can't malloc gfunc_table.");
  }else gfuncs=NULL;
  ngfuncs=fn;
  for(--fn,f=gf;fn>=0;fn--,f=f->next){ /* reverse order */
    offset_type *ot;
    gfuncs[fn].type=f->type;
    gfuncs[fn].alias=f->alias;
    gfuncs[fn].fname=f->fname;
    for(gn=0;gn<ngrids && f->gname!=grids[gn].name;gn++);
    if(gn==ngrids){
      fprintf(stderr,"Error: gfunction %s defined on non-existant grid\n",f->fname->n);
      fatal_error("Bad grid function declaration.");
    }
    gfuncs[fn].grd=&grids[gn];
    gfuncs[fn].tlev=f->tlev;
    for(gfuncs[fn].ntlevs=0,ot=gfuncs[fn].tlev;ot!=NULL;ot=ot->next,gfuncs[fn].ntlevs++);
    gfuncs[fn].desc=f->desc;
    if(gfuncs[fn].ntlevs<2 && gfuncs[fn].alias==1){
      fprintf(stderr,"Error: gfunction %s cannot be aliased since it has only one time level\n",
              gfuncs[fn].fname->n);
      fatal_error("Bad grid function declaration.");
    }
    if(gfuncs[fn].ntlevs==0){
      gfuncs[fn].ntlevs=1;
      gfuncs[fn].tlev=new_offset();
      gfuncs[fn].tlev->offset=FLAG;
    }
  }


#ifdef RNPLDEBUG
fprintf(stderr,"setup gfuncs\n");
#endif

 /* build table of derivative operators */
  for(dn=0,o=od;o!=NULL;o=o->next,dn++);
  if(dn!=0){
    if((dopers=(dop_table *)malloc(dn*sizeof(dop_table)))==NULL)
      fatal_error("Can't malloc dop_table.");
  }else dopers=NULL;
  nopers=dn;
  for(dn=0,o=od;dn<nopers;dn++,o=o->next){
    offset_type *ot;
    bounds *bd;
    dopers[dn].name=o->deriv->name;
    dopers[dn].expr=o->deriv->expr;
    if(dopers[dn].expr->type != IDEN){
      dop_to_string(stderr,o->deriv);
      fprintf(stderr,"\n");
      fatal_error("Specification error -- illegal derivative operator definition.");
    }
    dopers[dn].clst=o->deriv->clst;
    dopers[dn].op_expr=o->op_expr;
    dopers[dn].tlev=find_tlevs(o->op_expr);
    for(dopers[dn].ntlevs=0,ot=dopers[dn].tlev;ot!=NULL;ot=ot->next,dopers[dn].ntlevs++);
    dopers[dn].indx=find_indx(o->op_expr);
    for(dopers[dn].ndim=0,bd=dopers[dn].indx;bd!=NULL;bd=bd->next,dopers[dn].ndim++);
  }
  for(dn=0;dn<nopers;dn++){ /* check coordinate lists */
    int i,res=1,j;
    coord_list *n1;
   
    for(i=0,n1=dopers[dn].clst->c_names;i<dopers[dn].clst->rank;i++,n1=n1->next){
      res*=is_coord(n1->name);
    }
    if(!res){
      fprintf(stderr,"Error: Bad coordinates in derivative operator ");
      fprintf(stderr,"%s(",dopers[dn].name->n);
      ex_to_string(stderr,dopers[dn].expr);
      fprintf(stderr,",");
      for(i=0,n1=dopers[dn].clst->c_names;i<dopers[dn].clst->rank-1;i++,n1=n1->next)
        fprintf(stderr,"%s,",n1->name->n);
      fprintf(stderr,"%s).\n",n1->name->n);
      fatal_error("Bad derivative operator definition.");
    }
  }


#ifdef RNPLDEBUG
fprintf(stderr,"setup operators\n");
#endif

 /* build table of residuals */
  for(rn=0,r=rd;r!=NULL;r=r->next)
    for(rl=r->res;rl!=NULL;rl=rl->next,rn++);
  if(rn!=0){
    if((resids=(res_table *)malloc(rn*sizeof(res_table)))==NULL)
      fatal_error("Can't malloc res_table.");
  }else resids=NULL;
  nresids=rn;
  for(--rn,r=rd;rn>=0;r=r->next){ /* reverse order */
    for(rl=r->res;rl!=NULL;rl=rl->next,rn--){
      dop_ref_list *dl;
      gfunc_table *gf;
      if((gf=name_to_gfunc(r->name))==NULL){
        fprintf(stderr,"Error: residual defined for non-existant grid function %s\n",r->name->n);
        fatal_error("Bad residual definition.");
      }
      resids[rn].gf_refs=resids[rn].glob_gf=NULL;
      resids[rn].crds=resids[rn].glob_crds=NULL;
      resids[rn].cdifs=resids[rn].glob_cdifs=NULL;
      resids[rn].parms=resids[rn].glob_par=NULL;
      resids[rn].gfunc=gf;
      resids[rn].eval=r->eval;
      resids[rn].toff=r->toff;
      resids[rn].indx=r->indx;
      resids[rn].reg=rl->reg;
      resids[rn].etype=rl->etype;
      resids[rn].elst=rl->elst;
      resids[rn].output=0;
#ifdef RNPLDEBUG
fprintf(stderr,"residuals[%d] getting dop_refs, etype=%d\n",rn,resids[rn].etype);
#endif
      if(resids[rn].etype==1)
        resids[rn].dop_refs=find_dops(resids[rn].elst.expr);
      else resids[rn].dop_refs=find_dops_if(resids[rn].elst.ifst);
#ifdef RNPLDEBUG
fprintf(stderr,"residuals[%d] finished getting dop_refs\n",rn);
#endif
      for(dl=resids[rn].dop_refs;dl!=NULL;dl=dl->next){
        gfunc_ref_list *gl;
        gl=dl->gf_refs;
        if(gl!=NULL)
          dl->grd=gl->gfunc->grd;
        else dl->grd=NULL;
        while(gl!=NULL){
          if(gl->gfunc->grd != dl->grd){
            fprintf(stderr,"In residual for <%s>\n",resids[rn].gfunc->fname->n);
            fatal_error("Grid functions in derivative are defined on different grids.");
          }
          gl=gl->next;
        }
      }
    }
  }
 

#ifdef RNPLDEBUG
fprintf(stderr,"setup residuals\n");
#endif

 /* must come after residuals so get_size_one and get_size_all know about
     which functions have residuals */
 al=ad;
 na=new_attrib_dec();
 na->type=INT;
 na->encoding=ENCODEONE;
 na->name=name_lookup("out_gf");
 na->def=1;
 if(al)
   add_attrib(al,na);
 else al=ad=na;

 /* build table of attributes */
 for(nattribs=0;al!=NULL;al=al->next,nattribs++);
 if((attribs=(attrib_table *)malloc(nattribs*sizeof(attrib_table)))==NULL)
   fatal_error("Can't malloc attrib_table.");

#ifdef RNPLDEBUG
fprintf(stderr,"nattribs: %d  attribs: %p\n",nattribs,attribs);
#endif

 for(an=0,al=ad;an<nattribs;al=al->next,an++){
   scalar_list *p;

   attribs[an].type=al->type;
   switch(al->encoding){
     case ENCODEONE : attribs[an].size=get_size_one();
                      break;
     case ENCODEALL : attribs[an].size=get_size_all();
                      break;
   }
    attribs[an].encoding=al->encoding;
    attribs[an].name=al->name;
    attribs[an].def=al->def;
    switch(attribs[an].type){
      case FLOAT  :  attribs[an].def_val.f_ar=new_f_ar(attribs[an].size);
                    if(attribs[an].def && al->def_val){
											if(!al->def_val->next){ /* see if there is one default */
												for(i=attribs[an].size-1;i>=0;i--)
													attribs[an].def_val.f_ar[i]=al->def_val->sc.data.num;
											}else{ /* there should be correct number of values */
												for(p=al->def_val,i=attribs[an].size-1;i>=0 && p!=NULL;i--,p=p->next)
													attribs[an].def_val.f_ar[i]=p->sc.data.num;
												if(i>=0){
													fprintf(stderr,"The initialization vector for attribute %s is too short.\n",
																	attribs[an].name->n);
													fprintf(stderr,"It should contain %d elements\n",attribs[an].size);
													fatal_error("Specification error.");
												}
											}
                    }else if(attribs[an].def){
                      for(i=0;i<attribs[an].size;i++)
                        attribs[an].def_val.f_ar[i]=0.0;
                     }
                    break;
      case INT    :  attribs[an].def_val.i_ar=new_i_ar(attribs[an].size);
                    if(attribs[an].def && al->def_val){
											if(!al->def_val->next){ /* see if there is one default */
												for(i=attribs[an].size-1;i>=0;i--)
													attribs[an].def_val.i_ar[i]=al->def_val->sc.data.inum;
											}else{ /* there should be correct number of values */
												for(p=al->def_val,i=attribs[an].size-1;i>=0 && p!=NULL;i--,p=p->next)
													attribs[an].def_val.i_ar[i]=p->sc.data.inum;
												if(i>=0){
													fprintf(stderr,"The initialization vector for attribute %s is too short.\n",
																	attribs[an].name->n);
													fprintf(stderr,"It should contain %d elements\n",attribs[an].size);
													fatal_error("Specification error.");
												}
											}
                    }else if(attribs[an].def){
                      for(i=0;i<attribs[an].size;i++)
                        attribs[an].def_val.i_ar[i]=0;
                    }
                    break;
      case STRING  :  attribs[an].def_val.s_ar=new_s_ar(attribs[an].size);
                    if(attribs[an].def && al->def_val){
											if(!al->def_val->next){ /* see if there is one default */
												for(i=attribs[an].size-1;i>=0;i--)
													attribs[an].def_val.s_ar[i]=al->def_val->sc.data.str;
											}else{ /* there should be correct number of values */
												for(p=al->def_val,i=attribs[an].size-1;i>=0 && p!=NULL;i--,p=p->next)
														attribs[an].def_val.s_ar[i]=p->sc.data.str;
												if(i>=0){
													fprintf(stderr,"The initialization vector for attribute %s is too short.\n",
																	attribs[an].name->n);
													fprintf(stderr,"It should contain %d elements\n",attribs[an].size);
													fatal_error("Specification error.");
												}
											}
                    }else if(attribs[an].def){
                      for(i=0;i<attribs[an].size;i++)
                        attribs[an].def_val.s_ar[i]=NULL;
                    }
                    break;
    }

  }


#ifdef RNPLDEBUG
fprintf(stderr,"setup attributes\n");
#endif
/* set attributes from gfunc declarations */
	for(fn=ngfuncs-1,f=gf;f!=NULL;f=f->next,fn--){ /* loop over grid functions */
		attrib_set *a;
		attrib_table *atab;
		name_list *n;
		int atn,tlev,resn;
		char root[50],suffix[12];
		for(a=f->atts;a!=NULL;a=a->next){
			suffix[0]=0;
			sscanf(a->name,"%[^[][%[^]]",root,suffix);
			n=add_name(root);
			atab=name_to_attrib(n);
			if(atab){
				if(suffix[0]==0){
					if(atab->encoding==ENCODEONE){
						atn=fn;
					}else{
						atn=-1;
					}
				}else if(!strcmp(suffix,"res")){
					resn=resid_exists(f->fname);
					if(resn==-1){
						fprintf(stderr,"The declaration for grid function <%s> contains the\n",
										f->fname->n);
						fprintf(stderr,"assignment of a residual attribute <%s>, but no residual exists.\n",
										a->name);
						fatal_error("Specification error.");
					}
					if(!resids[resn].eval){
						fprintf(stderr,"The declaration for grid function <%s> contains the\n",
										f->fname->n);
						fprintf(stderr,"assignment of a residual attribute <%s>, but the residual is not evalutated.\n",
										a->name);
						fatal_error("Specification error.");
					}
					if(atab->encoding==ENCODEONE)
						atn=get_size_one_n(resn);
					else atn=get_size_all_n(resn);
				}else if(sscanf(suffix,"%d",&tlev)==1){
					if(atab->encoding==ENCODEONE){
						fprintf(stderr,"The declaration for grid function <%s> contains the\n",
										f->fname->n);
						fprintf(stderr,"assignment of an attribute <%s> for time level %d.\n",
										a->name,tlev);
						fprintf(stderr,"However, the attribute is defined as encodeone.\n");
						fatal_error("Specification error.");
					}
					if((atn=gflev_num(fn,tlev))<0){
						fprintf(stderr,"The declaration for grid function <%s> contains the\n",
										f->fname->n);
						fprintf(stderr,"assignment of an attribute <%s> for time level %d.\n",
										a->name,tlev);
						fprintf(stderr,"However, <%s> is not defined on time level %d.\n",f->fname->n,tlev);
						fatal_error("Specification error.");
					}
				}else{
					fprintf(stderr,"The declaration for grid function <%s> contains the\n",
									f->fname->n);
					fprintf(stderr,"assignment of an attribute <%s>, with an unknown suffix <%s>.\n",
									a->name,suffix);
					fatal_error("Specification error.");
				}
			}else{
				fprintf(stderr,"The declaration for grid function <%s> contains the\n",
				        f->fname->n);
				fprintf(stderr,"assignment of an undeclared attribute <%s>\n",
				        a->name);
				fatal_error("Specification error.");
			}
			if(atn>-1){
				switch(atab->type){
					case FLOAT :
						atab->def_val.f_ar[atn]=a->val.data.num;
						break;
					case INT :
						atab->def_val.i_ar[atn]=a->val.data.inum;
						break;
					case STRING :
						atab->def_val.s_ar[atn]=a->val.data.str;
						break;
				}
			}else{
				for(atn=gflev(fn);atn<gflev(fn)+gfuncs[fn].ntlevs;atn++){
					switch(atab->type){
						case FLOAT :
							atab->def_val.f_ar[atn]=a->val.data.num;
							break;
						case INT :
							atab->def_val.i_ar[atn]=a->val.data.inum;
							break;
						case STRING :
							atab->def_val.s_ar[atn]=a->val.data.str;
							break;
					}
				}
			}
		}
	}
	
 /* build table of initializations */
  for(i=0,ind=id;ind!=NULL;ind=ind->next)
     for(rl=ind->init;rl!=NULL;rl=rl->next,i++);
  ninits=i;
  if(ninits!=0){
    if((inits=(init_table *)malloc(i*sizeof(init_table)))==NULL)
      fatal_error("Can't malloc init_table.");
  }else inits=NULL;
  for(--i,ind=id;i>=0;ind=ind->next){ /* reverse order */
    for(rl=ind->init;rl!=NULL;rl=rl->next,i--){
      if((inits[i].gfunc=name_to_gfunc(ind->name))==NULL){
        fprintf(stderr,"Error: initialization defined for non-existant grid function %s\n",ind->name->n);
        fatal_error("Bad initialization definition.");
      }
      inits[i].gf_refs=inits[i].glob_gf=NULL;
      inits[i].crds=inits[i].glob_crds=NULL;
      inits[i].cdifs=inits[i].glob_cdifs=NULL;
      inits[i].parms=inits[i].glob_par=NULL;
      inits[i].output=0;
      inits[i].reg=rl->reg;
      inits[i].etype=rl->etype;
      inits[i].elst=rl->elst;
      if(inits[i].gfunc->ntlevs==1 && inits[i].gfunc->tlev->offset==FLAG)
      	inits[i].toff=FLAG;
      else inits[i].toff=ind->toff;
      /* need some error checking for undefined time levels */
    }
  }           

#ifdef RNPLDEBUG
fprintf(stderr,"setup initializations\n");
#endif

  /* build table of initializers */
  for(gn=0,u=iz;u!=NULL;u=u->next,gn++);
  niniters=gn;
  if(niniters!=0){
    if((initers=(update_table *)malloc(gn*sizeof(update_table)))==NULL)
      fatal_error("Can't malloc update_table for initializers.");
    for(--gn,u=iz;gn>=0;gn--,u=u->next){ /* reverse order */
      if(u->refs)
        initers[gn].header=1;
      else initers[gn].header=0;
      initers[gn].glob_gf=NULL;
      initers[gn].glob_crds=NULL;
      initers[gn].glob_cdifs=NULL;
      initers[gn].glob_par=NULL;
      initers[gn].work_refs=NULL;
      initers[gn].type=u->type;
      initers[gn].name=u->name;
      initers[gn].gfs=new_gfunc_tab();
      for(gft=initers[gn].gfs,nm=u->gfs;nm!=NULL;nm=nm->next){
        gft->gfunc=name_to_gfunc(nm->name);
        if(gft->gfunc==NULL){
          fprintf(stderr,"Error: initializer %d defined for non-existant grid function %s\n",gn,nm->name->n);
          fatal_error("Bad initializer definition.");
        }
        if(nm->next != NULL){
          gft->next=new_gfunc_tab();
          gft=gft->next;
        }else gft->next=NULL;
      }
      if(u->refs){
        ref_list *rfl;
        gfunc_ref_list *gl;
        coord_list *cl;
        gfunc_table *gf;
        offset_type *o;
        work_list *wk;
        param_ref_list *pr;
        
        for(rfl=u->refs;rfl!=NULL;rfl=rfl->next){
          switch(rfl->type){
            case  IDEN  :  
  #ifdef RNPLDEBUG
  fprintf(stderr,"initializers: rfl->type=IDEN\n");
  fprintf(stderr,"  rfl->data.nref->name=%s\n",rfl->data.nref->name->n);
  #endif
                          switch(name_type(rfl->data.nref->name)){
                            case  GFUNC  :  
  #ifdef RNPLDEBUG
  fprintf(stderr,"  %s is a GFUNC\n",rfl->data.nref->name->n);
  #endif
                                          gf=name_to_gfunc(rfl->data.nref->name);
                                          for(cl=rfl->data.nref->subs,i=0,o=gf->tlev;
                                              i<gf->ntlevs;i++,o=o->next){
  #ifdef RNPLDEBUG
  fprintf(stderr,"  toff=%d\n",o->offset);
  #endif
                                            gl=new_gfunc_ref();
                                            gl->gfunc=gf;
                                            gl->toff=o->offset;
                                            if(cl){
                                              if(i==0 && !cl->next)
                                                gl->array=1;
                                              gl->name=cl->name;
                                              cl=cl->next;
                                            }
                                            if(initers[gn].glob_gf)
                                              add_gfunc_ref(initers[gn].glob_gf,gl);
                                            else initers[gn].glob_gf=gl;
                                          }
                                          if(i==0){
                                            gl=new_gfunc_ref();
                                            gl->gfunc=gf;
                                            gl->toff=FLAG;
                                            if(initers[gn].glob_gf)
                                              add_gfunc_ref(initers[gn].glob_gf,gl);
                                            else initers[gn].glob_gf=gl;
                                          }
                                          break;
                            case  COORD  :  
  #ifdef RNPLDEBUG
  fprintf(stderr,"  %s is a COORD\n",rfl->data.nref->name->n);
  #endif
                                          cl=new_coord_list();
                                          cl->name=rfl->data.nref->name;
                                          if(initers[gn].glob_crds)
                                            add_name_ref(initers[gn].glob_crds,cl);
                                          else initers[gn].glob_crds=cl;
                                          break;
                            case  CDIF  :  
  #ifdef RNPLDEBUG
  fprintf(stderr,"  %s is a CDIF\n",rfl->data.nref->name->n);
  #endif
                                          cl=new_coord_list();
                                          cl->name=rfl->data.nref->name;
                                          if(initers[gn].glob_cdifs)
                                            add_name_ref(initers[gn].glob_cdifs,cl);
                                          else initers[gn].glob_cdifs=cl;
                                          break;
                            case  PARAM  :  
  #ifdef RNPLDEBUG
  fprintf(stderr,"  %s is a PARAM\n",rfl->data.nref->name->n);
  #endif
                                          pr=new_param_ref();
                                          for(i=0;i<nparams && 
                                              params[i].name!=rfl->data.nref->name;i++);
                                          if(i==nparams)
                                            fatal_error("Funky goings on.");
                                          pr->par=&params[i];
                                          if(initers[gn].glob_par)
                                            add_param_ref(initers[gn].glob_par,pr);
                                          else initers[gn].glob_par=pr;
                                          break;
                          }
                          break;
            case  WORK  :  
  #ifdef RNPLDEBUG
  fprintf(stderr,"initializers: rfl->type=WORK\n");
  fprintf(stderr,"  rfl->data.work->num=%d\n",rfl->data.work->num);
  #endif
                          wk=new_work_list();
                          wk->work=rfl->data.work;
                          if(initers[gn].work_refs)
                            add_work_ref(initers[gn].work_refs,wk);
                          else initers[gn].work_refs=wk;
                          break;
          }
        }
      }
    }
  }else if(ninits>0){
    niniters=1;
    if((initers=(update_table *)malloc(niniters*sizeof(update_table)))==NULL)
      fatal_error("Can't malloc update_table for initializers.");
    initers[gn].header=0;
    initers[gn].glob_gf=NULL;
    initers[gn].glob_crds=NULL;
    initers[gn].glob_cdifs=NULL;
    initers[gn].glob_par=NULL;
    initers[gn].work_refs=NULL;
    initers[gn].type=name_lookup("auto");
    initers[gn].name=NULL;
    initers[gn].gfs=new_gfunc_tab();
    for(gft=initers[gn].gfs,i=0;i<ninits;i++){
      gft->gfunc=inits[i].gfunc;
      if(i<ninits-1){
        gft->next=new_gfunc_tab();
        gft=gft->next;
      }else gft->next=NULL;
    }
  }else initers=NULL;

#ifdef RNPLDEBUG
fprintf(stderr,"setup initializers\n");
#endif

  loop_driver=ld;
  if(loop_driver==NULL)
    loop_driver=add_name("standard");

#ifdef RNPLDEBUG
fprintf(stderr,"setup loop driver\n");
#endif
    
  /* build table of updates */
  for(gn=0,u=ud;u!=NULL;u=u->next,gn++);
  nupdates=gn;
  if(nupdates!=0){
    if((updates=(update_table *)malloc(gn*sizeof(update_table)))==NULL)
      fatal_error("Can't malloc update_table.");
  }else updates=NULL;
  for(--gn,u=ud;gn>=0;gn--,u=u->next){ /* reverse order */
    if(u->refs)
      updates[gn].header=1;
    else updates[gn].header=0;
    updates[gn].glob_gf=NULL;
    updates[gn].glob_crds=NULL;
    updates[gn].glob_cdifs=NULL;
    updates[gn].glob_par=NULL;
    updates[gn].work_refs=NULL;
    updates[gn].type=u->type;
    updates[gn].name=u->name;
    updates[gn].gfs=new_gfunc_tab();
    for(gft=updates[gn].gfs,nm=u->gfs;nm!=NULL;nm=nm->next){
      gft->gfunc=name_to_gfunc(nm->name);
      if(gft->gfunc==NULL){
        fprintf(stderr,"Error: update %d defined for non-existant grid function %s\n",gn,nm->name->n);
        fatal_error("Bad update definition.");
      }
      if(nm->next != NULL){
        gft->next=new_gfunc_tab();
        gft=gft->next;
      }else gft->next=NULL;
    }
    if(u->refs){
      ref_list *rfl;
      gfunc_ref_list *gl;
      coord_list *cl;
      gfunc_table *gf;
      offset_type *o;
      work_list *wk;
      param_ref_list *pr;
      
      for(rfl=u->refs;rfl!=NULL;rfl=rfl->next){
        switch(rfl->type){
          case  IDEN  :  
#ifdef RNPLDEBUG
fprintf(stderr,"updates: rfl->type=IDEN\n");
fprintf(stderr,"  rfl->data.nref->name=%s\n",rfl->data.nref->name->n);
#endif
                        switch(name_type(rfl->data.nref->name)){
                          case  GFUNC  :  
#ifdef RNPLDEBUG
fprintf(stderr,"  %s is a GFUNC\n",rfl->data.nref->name->n);
#endif
                                        gf=name_to_gfunc(rfl->data.nref->name);
                                        for(cl=rfl->data.nref->subs,i=0,o=gf->tlev;
                                            i<gf->ntlevs;i++,o=o->next){
#ifdef RNPLDEBUG
fprintf(stderr,"  toff=%d\n",o->offset);
#endif
                                          gl=new_gfunc_ref();
                                          gl->gfunc=gf;
                                          gl->toff=o->offset;
                                          if(cl){
                                            if(i==0 && !cl->next)
                                              gl->array=1;
                                            gl->name=cl->name;
                                            cl=cl->next;
                                          }
                                          if(updates[gn].glob_gf)
                                            add_gfunc_ref(updates[gn].glob_gf,gl);
                                          else updates[gn].glob_gf=gl;
                                        }
                                        if(i==0){
                                          gl=new_gfunc_ref();
                                          gl->gfunc=gf;
                                          gl->toff=FLAG;
                                          if(updates[gn].glob_gf)
                                            add_gfunc_ref(updates[gn].glob_gf,gl);
                                          else updates[gn].glob_gf=gl;
                                        }
                                        break;
                          case  COORD  :  
#ifdef RNPLDEBUG
fprintf(stderr,"  %s is a COORD\n",rfl->data.nref->name->n);
#endif
                                        cl=new_coord_list();
                                        cl->name=rfl->data.nref->name;
                                        if(updates[gn].glob_crds)
                                          add_name_ref(updates[gn].glob_crds,cl);
                                        else updates[gn].glob_crds=cl;
                                        break;
                          case  CDIF  :  
#ifdef RNPLDEBUG
fprintf(stderr,"  %s is a CDIF\n",rfl->data.nref->name->n);
#endif
                                        cl=new_coord_list();
                                        cl->name=rfl->data.nref->name;
                                        if(updates[gn].glob_cdifs)
                                          add_name_ref(updates[gn].glob_cdifs,cl);
                                        else updates[gn].glob_cdifs=cl;
                                        break;
                          case  PARAM  :  
#ifdef RNPLDEBUG
fprintf(stderr,"  %s is a PARAM\n",rfl->data.nref->name->n);
#endif
                                        pr=new_param_ref();
                                        for(i=0;i<nparams && 
                                            params[i].name!=rfl->data.nref->name;i++);
                                        if(i==nparams)
                                          fatal_error("Funky goings on.");
                                        pr->par=&params[i];
                                        if(updates[gn].glob_par)
                                          add_param_ref(updates[gn].glob_par,pr);
                                        else updates[gn].glob_par=pr;
                                        break;
                        }
                        break;
          case  WORK  :  
#ifdef RNPLDEBUG
fprintf(stderr,"updates: rfl->type=WORK\n");
fprintf(stderr,"  rfl->data.work->num=%d\n",rfl->data.work->num);
#endif
                        wk=new_work_list();
                        wk->work=rfl->data.work;
                        if(updates[gn].work_refs)
                          add_work_ref(updates[gn].work_refs,wk);
                        else updates[gn].work_refs=wk;
                        break;
        }
      }
    }
  }

#ifdef RNPLDEBUG
fprintf(stderr,"setup updates\n");
#endif

}
