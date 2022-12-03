/* data structures and globals for RNPL */
/* $Header: /home/cvs/rnpl/src/rnpl.h,v 1.1.1.1 2013/07/09 00:38:27 cvs Exp $ */
/* Copyright (c) 1994-1996 by Robert L. Marsa */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define IVEL 4

/* typedefs */

struct dp;
struct gfc;
struct nml;
struct fn;
struct cd;
struct gdt;
struct ifsta;

typedef struct stl {
  int type;
  char *str;
  struct stl *next;
} statement_list;

typedef struct {
  int type;
  union {
    char *str;
    int inum;
    double num;
  } data;
} scalar;

typedef struct sl {
  scalar sc;
  struct sl *next;
} scalar_list;

typedef struct nd {
  int type;
  union {
    char op;
    double num;
    int inum;
    struct nml *name;
    struct dp *deriv;
    struct fn *func;
    struct gfc *gfunc;
    struct cd *coord;
  } data;
  struct nd *left;
  struct nd *right;
} node;

typedef struct {
   int type;
   union {
      int offset;
      node *expr;
   }val;
} indel;

typedef struct indl {
   indel *ind;
   struct indl *next;
} index_list;

typedef struct nml{
  char *n;
  struct nml *next;
} name_list;

typedef struct {
   int ato;
   int num;
   node *expr;
} work_array;

typedef struct wl {
   work_array *work;
   struct wl *next;
} work_list;

typedef union {
  char **s_ar;
  int *i_ar;
  double *f_ar;
} vector;

typedef struct vsz {
   int dim;
   int *size;
} v_size;

typedef struct ivl {
   int low,high,mod;
   struct ivl *next;
} ivec_list;

typedef struct pd {
  int type;
  v_size *size;
   int con;
  name_list *name;
  int def;
  vector def_val;
  struct pd *next;
} param_dec;

typedef struct {
  int type;
  v_size *size;
   int con;
  name_list *name;
  int def;
  vector def_val;
} param_table;

typedef struct prl {
   param_table *par;
   struct prl *next;
} param_ref_list;

typedef struct as {
   char *name;
   scalar val;
   struct as *next;
} attrib_set;

typedef struct ad {
  int type;
  int encoding;
  name_list *name;
  int def;
  scalar_list *def_val;
  struct ad *next;
} attrib_dec;

typedef struct {
  int type;
  int size;
  int encoding;
  name_list *name;
  int def;
  vector def_val;
} attrib_table;

typedef struct cd {
  name_list *name;
  struct gdt *grd;
  indel *indx;
} coord_ref;

typedef struct crd {
  name_list *name;
  struct crd *next;
} coord_list;

typedef struct {
   name_list *name;
   coord_list *subs;
} name_ref;

typedef struct refl{
   int type;
   union {
      name_ref *nref;
      work_array *work;
   } data;
   struct refl *next;
} ref_list;

typedef struct crdd {
  int rank;
  coord_list *c_names;
   name_list *name;
   struct crdd *next;
} coord_dec;

typedef struct {
   int rank;
   coord_list *c_names;
   name_list *name;
} coord_table;

typedef struct ir {
  node *lower;
  node *upper;
  int inc;
  struct ir *next;
} i_reg;

typedef struct cr {
  name_list *lower;
  name_list *upper;
  struct cr *next;
} c_reg;

typedef struct {
  int rank;
  i_reg *bounds;
  c_reg *limits;
} region;

typedef struct tof {
  int offset;
  struct tof *next;
} offset_type;

typedef struct gd {
  int type;
   name_list *cname;
  name_list *name;
   coord_list *clst;
  i_reg *ireg;
  c_reg *creg;
  struct gd *next;
} grid_list;

typedef struct gdt{
  int type;
   coord_table *crds;
  name_list *name;
   coord_list *clst;
  region reg;
} grid_table;

typedef struct gfc {
  int toff;
  name_list *name;
  index_list *indx;
  struct gdt *grd;
} gfunction;

typedef struct fn {
  name_list *name;
  node *expr;
} func;

typedef struct dp{
  int expand;
  name_list *name;
  node *expr;
  coord_table *clst;
} d_op;

typedef struct op {
  d_op *deriv;
  node *op_expr;
  struct op *next;
} operator;

typedef struct bds{
  int upper;
  int lower;
  struct bds *next;
} bounds;

typedef struct {
  name_list *name;
  node *expr;
  coord_table *clst;
  node *op_expr;
  int ntlevs;
  offset_type *tlev;
  int ndim;
  bounds *indx;
} dop_table;

typedef struct fd {
  int type;
   int alias;
  name_list *fname;
  name_list *gname;
  offset_type *tlev;
  char *desc;
   attrib_set *atts;
  struct fd *next;
} gfunc_list;

typedef struct {
  int type;
   int alias;
   int neediditer;
  name_list *fname;
  grid_table *grd;
  int ntlevs;
  offset_type *tlev;
  char *desc;
} gfunc_table;

typedef struct gfr {
  gfunc_table *gfunc;
  int toff;
  int array;
   name_list *name;
  struct gfr *next;
} gfunc_ref_list;

typedef struct gftbl {
  gfunc_table *gfunc;
   struct gftbl *next;
} gfunc_tab_list;

typedef struct drl {
  dop_table *dop;
  gfunc_ref_list *gf_refs;
  grid_table *grd;
  struct drl *next;
} dop_ref_list;

typedef union {
   node *expr;
   struct ifsta *ifst;
} elstat;

typedef struct ifsta {
   node *lexpr;
   node *expr;
   int etype; /* (0, 1, 2) -> (none, expr, ifstat) */
   elstat elst;
} ifstat;

typedef struct rlst {
   i_reg *reg;
   int etype;
   elstat elst;
   struct rlst *next;
} res_list;

typedef struct rd {
   int eval;
  int toff;
  offset_type *indx;
  name_list *name;
   res_list *res;
  struct rd *next;
} residual;

typedef struct {
  int output;
  int eval;
  int toff;
  offset_type *indx;
  gfunc_table *gfunc;
  i_reg *reg;
   int etype;
   elstat elst;
  dop_ref_list *dop_refs;
  gfunc_ref_list *gf_refs;
  gfunc_ref_list *glob_gf;
   coord_list *crds;
   coord_list *glob_crds;
   coord_list *cdifs;
   coord_list *glob_cdifs;
   param_ref_list *parms;
   param_ref_list *glob_par;
} res_table;

typedef struct upd {
  name_list *type;
  name_list *name;
  coord_list *gfs;
  ref_list *refs;
  struct upd *next;
} update;

typedef struct {
   int header;
  gfunc_tab_list *gfs;
  name_list *type;
  name_list *name;
  gfunc_ref_list *glob_gf;
   param_ref_list *glob_par;
   coord_list *glob_crds;
   coord_list *glob_cdifs;
   work_list *work_refs;
} update_table;

typedef struct id {
   int toff;
  name_list *name;
  res_list *init;
  struct id *next;
} init_dec;

typedef struct {
  int output;
  int toff;
  gfunc_table *gfunc;
  i_reg *reg;
   int etype;
   elstat elst;
  gfunc_ref_list *gf_refs;
  gfunc_ref_list *glob_gf;
   coord_list *crds;
   coord_list *glob_crds;
   coord_list *cdifs;
   coord_list *glob_cdifs;
   param_ref_list *parms;
   param_ref_list *glob_par;
} init_table;

typedef struct qn {
  node *expr;
  struct qn *next;
} qnode;

typedef struct {
  qnode *head;
  qnode *tail;
} qheader;

#define TMPSIZE 2048
#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

void cray_call(char *s);
void sun_call(char *s);
void linux_call(char *s);
void fort_call(char *s);

void fatal_error(const char *msg);
int fort_out_const(char *s, const double d);
void fort_out(FILE *fp, const char *s);
void fortcom_out(FILE *fp, const char *s);

int mystrcmp(const char *s1, const char *s2);
void dump_name(const char *s, const name_list *nm);
name_list *add_name(const char *nm);
name_list *name_lookup(const char *nm);
int vsize(const v_size *vs);

attrib_set *new_attrib_set();
ivec_list *new_ivec_list();
param_ref_list *new_param_ref();
index_list *new_index_list();
indel *new_indel();
work_list *new_work_list();
work_array *new_work_array();
name_ref *new_name_ref();
ref_list *new_ref_list();
statement_list *new_statement_list();
scalar_list *new_scalar_list();
v_size *new_v_size();
param_dec *new_param_dec();
attrib_dec *new_attrib_dec();
init_dec *new_init_dec();
int *new_i_ar(const int i);
double *new_f_ar(const int i);
char **new_s_ar(const int i);
char *new_string(const int i);
name_list *new_name();
coord_ref *new_coord();
gfunction *new_gfunc();
d_op *new_dop();
node *new_node();
offset_type *new_offset();
func *new_func();
coord_list *new_coord_list();
coord_dec *new_coord_dec();
coord_table *new_coord_tab();
update *new_update();
bounds *new_bounds();
dop_ref_list *new_dop_ref();
gfunc_ref_list *new_gfunc_ref();
gfunc_tab_list *new_gfunc_tab();
grid_list *new_grid_list();
gfunc_list *new_gfunc_list();
operator *new_operator();
res_list *new_res_list();
residual *new_residual();
i_reg *new_i_reg();
c_reg *new_c_reg();
ifstat *new_ifstat();

void gfunc_suffix(FILE *fp,const int offset);
void gfunc_suffix_f(FILE *fp,const int offset);
int get_priority(const char c);
void do_left_paren(FILE *fp, node *expr, char *c);
void do_right_paren(FILE *fp, node *expr, char *c);
void ex_to_string(FILE *fp, node *expr);
void ex_to_string_wk(FILE *fp, node *expr);
void do_left_paren_f(FILE *fp, node *expr, char *c);
void do_right_paren_f(FILE *fp, node *expr, char *c);
void ex_to_string_f(FILE *fp, node *expr);
void ireg_to_string(FILE *fp, node *expr, const name_list *nm);
void ireg_to_string_f(FILE *fp, node *expr, const name_list *nm);
void dop_to_string(FILE *fp,d_op *d);
void func_to_string(FILE *fp,func *f);
void func_to_string_wk(FILE *fp,func *f);
void func_to_string_f(FILE *fp,func *f);
void coord_to_string(FILE *fp,coord_ref *cd);
void coord_to_string_f(FILE *fp,coord_ref *cd);
void gfunc_to_string(FILE *fp,gfunction *f);
void gfunc_to_string_f(FILE *fp,gfunction *f);
void array_bounds_to_string(FILE *fp, const grid_table *gr, const int ar);
void array_reff0_to_string(FILE *fp,const grid_table *gr);
void array_reff_to_string(FILE *fp, const gfunction *f, const grid_table *gr);
void array_ref0_to_string(FILE *fp,const grid_table *gr);
void array_ref_to_string(FILE *fp,const gfunction *f,const grid_table *gr);
void array_ref0_to_string_f(FILE *fp,const grid_table *gr);
void array_ref_to_string_f(FILE *fp,const gfunction *f,const grid_table *gr);
void grid_size(FILE *fp,const grid_table *gr);
void grid_size_r(FILE *fp,const grid_table *gr);

void show_name_type(const name_list *nm);
int name_type(const name_list *nm);
int is_upfile(const name_list *nm);
int is_update(const name_list *nm);
int is_func(const name_list *nm);
int is_dop(const name_list *nm);
int is_attrib(const name_list *nm);
int is_coord_sys(const name_list *nm);
int is_grid(const name_list *nm);
int is_cdif(const name_list *nm);
int is_gbase(const name_list *nm);
int is_param(const name_list *nm);
int is_const(const name_list *nm);
attrib_table *name_to_attrib(const name_list *nm);
gfunc_table *name_to_gfunc(const name_list *nm);
dop_table *find_dop(const name_list *nm, const coord_table *cl);
int resid_exists(const name_list *nm);
int init_exists(const name_list *nm);
int initoff_exists(const name_list *nm, const int toff);
int initer_exists(const name_list *nm);
int in_clst(const name_list *nm, const coord_table *c1);
int is_coord(const name_list *nm);
int is_space(const name_list *nm);
int rank(const name_list *nm);
int compare_ireg(const i_reg *i1, const i_reg *i2);
int compare_ctab(const coord_table *c1, const coord_table *c2);
int compare_offset(const offset_type *i1, const offset_type *i2);
int compare_index(const index_list *i1, const index_list *i2);
int compare_expr(const node *e1, const node *e2);
void expand_expr(node *expr);
void expand_ifstat(ifstat *ifst);
void evaluate_expr(node *expr);
void evaluate_ifstat(ifstat *ifst);
void offset_expr(const int toff, offset_type *ind, node *expr);
void offset_ifstat(const int toff, offset_type *ind, ifstat *ifst);
void simplify_expr(node *expr);
void simplify_ifstat(ifstat *ifst);
void make_expand(node *expr);
int check_expr(node *expr);
int check_ifstat(ifstat *ifst);
node *deriv_wrt_gfunc(node *expr, node *gf);
void eval_wrt_gfunc(node *expr, node *gf);
void del_clst(coord_list *cl);
coord_list *dup_clst(coord_list *cl);
coord_table *dup_ctab(coord_table *cl);
gfunc_ref_list *dup_gf_ref(gfunc_ref_list *grl);
param_ref_list *dup_param_ref(param_ref_list *prl);
work_list *dup_work_list(work_list *wl);
indel *dup_indel(indel *indx);
index_list *dup_indx(index_list *indx);
node *dup_expr(node *expr);
void del_expr(node *expr);
grid_table *get_grid(node *ex);
void set_grid(grid_table *grd, node *ex);
void name_subst(name_list *nm, grid_table *grd, node *ex, const name_list *s_nm);
void coord_subst(coord_ref *crd, node *ex);
void gfunc_subst(gfunction *gf, node *ex, const name_list *s_nm);
void expr_subst(node *e1, node *ex, const name_list *s_nm);
void gfunc_stuff(node *e1, node *ex);
qheader *init_queue();
void enqueue(qheader *qh, node *expr);
node *dequeue(qheader *qh);
void add_offset(offset_type *l, offset_type *o);
void add_dop_ref(dop_ref_list *l, dop_ref_list *o);
void add_gfunc_ref(gfunc_ref_list *l, gfunc_ref_list *o);
void add_name_ref(coord_list *l, coord_list *o);
void add_param_ref(param_ref_list *l, param_ref_list *o);
void add_param(param_dec *l, param_dec *o);
void add_attrib(attrib_dec *l, attrib_dec *o);
void add_work_ref(work_list *l, work_list *o);
offset_type *find_tlevs(const node *n);
bounds *find_indx(const node *n);
gfunc_ref_list *find_gfs(const node *ex);
dop_ref_list *find_dops(const node *ex);
dop_ref_list *find_dops_if(const ifstat *ifst);
gfunc_ref_list *find_gfuncs(const node *ex);
gfunc_ref_list *find_gfuncs_if(const ifstat *ifst);
coord_list *find_coords(const node *ex);
coord_list *find_coords_if(const ifstat *ifst);
coord_list *find_cdifs(const node *ex);
coord_list *find_cdifs_if(const ifstat *ifst);
param_ref_list *find_params(const node *ex);
param_ref_list *find_params_if(const ifstat *ifst);
int gflev_num(const int gfnum, const int toff);
int gflev(const int gfnum);
int get_size_one_n(const int len);
int get_size_one();
int get_size_all_n(const int len);
int get_size_all();
int gptr_to_index(const grid_table *gr);
int update_to_index(const update_table *up);
int initer_to_index(const update_table *up);
int gfunc_to_index(const gfunc_table *gf);
int offset_to_index(const int tof, offset_type *ofl);
int gbase_to_gindex(name_list *nm);
int check_gen_expr(node *ex);
int check_gen_ifstat(ifstat *ifst);
int check_simp_expr(node *ex);
int check_log_expr(node *ex);

void resid_header(FILE *fp, const res_table *rs);
void resid_call(FILE *fp, const res_table *rs);
void update_header(FILE *fp, const update_table *up);
void update_call(FILE *fp, const update_table *up);
void output_resid_stat(FILE *fp, const res_table *rs);
void write_single_stat(FILE *fp, const res_table *rs, node *e);
void output_update_stat(FILE *fp, const res_table *rs);
void declare_coord_difs(FILE *fp);
void declare_parameters(FILE *fp);
void declare_grids(FILE *fp);
void declare_gfuncs(FILE *fp);
void declare_attributes(FILE *fp);
void code_residuals(FILE *fp);
void code_residuals_f77(FILE *fp);
void code_residuals_c(FILE *fp);
void swap_top(FILE *fp);
void swap_top_f77(FILE *fp);
void swap_top_heaer(FILE *fp);
void swap_top_call(FILE *fp);
void swap_top_c(FILE *fp);
void swap_levels(FILE *fp);
void swap_levels_f77(FILE *fp);
void swap_header(FILE *fp);
void swap_call(FILE *fp);
void swap_levels_c(FILE *fp);
void one_step(FILE *fp);
void one_step_f77(FILE *fp);
void one_step_c(FILE *fp);
void update_helpers(FILE *fp);
void update_helpers_f77(FILE *fp);
void initial_guess_proto(FILE *fp);
void initial_guess_call(FILE *fp);
void calc_resid_proto(FILE *fp);
void calc_resid_call(FILE *fp);
void take_step_proto(FILE *fp);
void take_step_call(FILE *fp);
void update_helpers_c(FILE *fp);
void code_updates(FILE *fp);
void init_params_attribs(FILE *fp);
void init_params_attribs_f77(FILE *fp);
void params_header(FILE *fp);
void params_call(FILE *fp);
void attribs_header(FILE *fp);
void attribs_call(FILE *fp);
void check_params_header(FILE *fp);
void check_params_call(FILE *fp);
void init_params_attribs_c(FILE *fp);
void init_coord_difs(FILE *fp);
void init_coord_difs_f77(FILE *fp);
void init_coord_difs_c(FILE *fp);
void init_grids(FILE *fp);
void init_grids_f77(FILE *fp);
void init_grids_c(FILE *fp);
void init_gfuncs(FILE *fp);
void init_gfuncs_f77(FILE *fp);
void init_gfuncs_c(FILE *fp);
void read_state(FILE *fp,char *iname);
void read_state_f77(FILE *fp,char *iname);
void read_state_header(FILE *fp);
void read_state_call(FILE *fp);
void read_state_c(FILE *fp,char *iname);
void read_params_attribs(FILE *fp);
void read_params_attribs_f77(FILE *fp);
void write_params_attribs(FILE *fp);
void write_params_attribs_f77(FILE *fp);
void cleanup(FILE *fp);
void handler(FILE *fp);
void handler_f77(FILE *fp);
void handler_c(FILE *fp);
void dump_state(FILE *fp, int flag);
void dump_state_f77(FILE *fp);
void dump_state_header(FILE *fp);
void dump_state_call(FILE *fp);
void dump_state_c(FILE *fp, int flag);
void output_header(FILE *fp);
void output_call(FILE *fp);
void output_func(FILE *fp);
void output_func_f77(FILE *fp);
void output_func_c(FILE *fp);
void init_header(FILE *fp, const update_table *up);
void init_call(FILE *fp, const update_table *up);
void output_init_stat(FILE *fp, init_table *in);
void gen_gfuncs(FILE *fp);
void gen_gfuncs_f77(FILE *fp);
void gen_gfuncs_c(FILE *fp);
void code_iteration(FILE *fp);
void code_iteration_f77(FILE *fp);
void update_iter_header_args(FILE *fp);
void update_iter_call_args(FILE *fp);
void code_iteration_c(FILE *fp);
void make_c_header(FILE *fp);
void make_f77_header(FILE *fp);
void make_c_init_main(FILE *fp,char *iname);
void make_f77_init_main(FILE *fp,char *iname);
void make_par_c_main(FILE *fp, char *oname);
void make_c_main(FILE *fp, char *oname);
void make_par_f77_main(FILE *fp, char *oname);
void make_f77_main(FILE *fp, char *oname);
void make_iter_loop_c(FILE *fp);
void make_iter_loop_f77(FILE *fp);
void make_stand_loop_c(FILE *fp);
void make_stand_loop_f77(FILE *fp);
void output_gfuni0();

#define FLAG 100000

#ifdef _SUPPORT_

/* globals */
name_list *Ln,*Sin,*Cos,*Exp,*Tan,*Sqrt,*Sinh,*Cosh,*Tanh,*ASin,*ACos,*ATan,*Abs;
int ncoords; /* number of coordinate systems */
int nparams; /* number of parameters */
int ngrids; /* number of grids */
int ngfuncs; /* number of functions */
int nopers; /* number of derivative operators */
int nresids; /* number of residuals */
int language; /* output language */
int nupdates; /* number of updates */
int nattribs; /* number of attributes */
int ninits; /* number of initializations */
int niniters; /* number of initializers */
int neediditer=0; /* need initial data iteration */

coord_table *coords; /* table of coordinate systems */
name_list ***coord_difs; /* table of coordinate differentials */
name_list ***grid_base; /* table of grid bases */
param_table *params; /* table of parameters */
grid_table *grids; /* table of grids */
gfunc_table *gfuncs; /* table of functions defined on the grids */
attrib_table *attribs; /* table of attributes */
dop_table *dopers; /* table of derivative operators */
res_table *resids; /* table of residuals */
update_table *updates; /* table of updates */
init_table *inits; /* table of initializations */
update_table *initers; /* table of initializers */
name_list str_tbl; /* list of strings */
statement_list *stmts; /* list of statements */
name_list *loop_driver; /* name of driver file */
work_list *static_work; /* list of static work arrays */
gfunc_ref_list *update_glob_gf; /* global list of gfuncs in updates */
param_ref_list *update_glob_par; /* global list of params in updates */
coord_list *update_glob_crds; /* global list of coords in updates */
coord_list *update_glob_cdifs; /* global list of cdifs in updates */
work_list *update_auto_work; /* global list of work arrays in updates */

int fort_const_D=1,adapt=0,parallel=0;
#else

/* globals */
extern name_list *Ln,*Sin,*Cos,*Exp,*Tan,*Sqrt,*Sinh,*Cosh,*Tanh,*ASin,*ACos,*ATan,*Abs;
extern int ncoords; /* number of coordinate systems */
extern int nparams; /* number of parameters */
extern int ngrids; /* number of grids */
extern int ngfuncs; /* number of functions */
extern int nopers; /* number of derivative operators */
extern int nresids; /* number of residuals */
extern int language; /* output language */
extern int nupdates; /* number of updates */
extern int nattribs; /* number of attributes */
extern int ninits; /* number of initializations */
extern int niniters; /* number of initializers */
extern int neediditer; /* need initial data iteration */

extern coord_table *coords; /* table of coordinate systems */
extern name_list ***coord_difs; /* table of coordinate differentials */
extern name_list ***grid_base; /* table of grid bases */
extern param_table *params; /* table of parameters */
extern grid_table *grids; /* table of grids */
extern gfunc_table *gfuncs; /* table of functions defined on the grids */
extern attrib_table *attribs; /* table of attributes */
extern dop_table *dopers; /* table of derivative operators */
extern res_table *resids; /* table of residuals */
extern update_table *updates; /* table of updates */
extern init_table *inits; /* table of initializations */
extern update_table *initers; /* table of initializers */
extern name_list str_tbl; /* list of strings */
extern statement_list *stmts; /* list of statements */
extern name_list *loop_driver; /* name of driver file */
extern work_list *static_work; /* list of static work arrays */
extern gfunc_ref_list *update_glob_gf; /* global list of gfuncs in updates */
extern param_ref_list *update_glob_par; /* global list of params in updates */
extern coord_list *update_glob_crds; /* global list of coords in updates */
extern coord_list *update_glob_cdifs; /* global list of cdifs in updates */
extern work_list *update_auto_work; /* global list of work arrays in updates */

extern int fort_const_D,adapt,parallel;
#endif
