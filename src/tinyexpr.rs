// #![allow(dead_code,
//          mutable_transmutes,
//          non_camel_case_types,
//          non_snake_case,
//          non_upper_case_globals,
//          unused_mut)]
// // #![feature(extern_types, libc, offset_to)]
// #![allow(deprecated)] 
// #![feature(start)]


#![feature(libc)]
#![feature(const_ptr_null)]
#![feature(offset_to)]
#![feature(const_ptr_null_mut)]
#![feature(extern_types)]
#![feature(asm)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(mutable_transmutes)]
#![allow(unused_mut)]


 use libc;
extern "C" {
    pub type _IO_FILE_plus;
    #[no_mangle]
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    #[no_mangle]
    fn memcpy(_: *mut libc::c_void, _: *const libc::c_void, _: libc::c_ulong)
     -> *mut libc::c_void;
    #[no_mangle]
    fn memset(_: *mut libc::c_void, _: libc::c_int, _: libc::c_ulong)
     -> *mut libc::c_void;
    #[no_mangle]
    fn fmod(_: libc::c_double, _: libc::c_double) -> libc::c_double;
    #[no_mangle]
    fn pow(_: libc::c_double, _: libc::c_double) -> libc::c_double;
    #[no_mangle]
    fn strncmp(_: *const libc::c_char, _: *const libc::c_char,
               _: libc::c_ulong) -> libc::c_int;
    #[no_mangle]
    fn tanh(_: libc::c_double) -> libc::c_double;
    #[no_mangle]
    fn tan(_: libc::c_double) -> libc::c_double;
    #[no_mangle]
    fn sqrt(_: libc::c_double) -> libc::c_double;
    #[no_mangle]
    fn sinh(_: libc::c_double) -> libc::c_double;
    #[no_mangle]
    fn sin(_: libc::c_double) -> libc::c_double;
    #[no_mangle]
    fn log10(_: libc::c_double) -> libc::c_double;
    #[no_mangle]
    fn log(_: libc::c_double) -> libc::c_double;
    #[no_mangle]
    fn floor(_: libc::c_double) -> libc::c_double;
    #[no_mangle]
    fn exp(_: libc::c_double) -> libc::c_double;
    #[no_mangle]
    fn cosh(_: libc::c_double) -> libc::c_double;
    #[no_mangle]
    fn cos(_: libc::c_double) -> libc::c_double;
    #[no_mangle]
    fn ceil(_: libc::c_double) -> libc::c_double;
    #[no_mangle]
    fn atan2(_: libc::c_double, _: libc::c_double) -> libc::c_double;
    #[no_mangle]
    fn atan(_: libc::c_double) -> libc::c_double;
    #[no_mangle]
    fn asin(_: libc::c_double) -> libc::c_double;
    #[no_mangle]
    fn acos(_: libc::c_double) -> libc::c_double;
    #[no_mangle]
    fn fabs(_: libc::c_double) -> libc::c_double;
    #[no_mangle]
    fn strtod(__nptr: *const libc::c_char, __endptr: *mut *mut libc::c_char)
     -> libc::c_double;
    #[no_mangle]
    fn free(__ptr: *mut libc::c_void) -> ();
    #[no_mangle]
    fn printf(_: *const libc::c_char, ...) -> libc::c_int;
    #[no_mangle]
    static mut signgam: libc::c_int;
    #[no_mangle]
    static mut _LIB_VERSION: _LIB_VERSION_TYPE;
    #[no_mangle]
    static mut _IO_2_1_stdin_: _IO_FILE_plus;
    #[no_mangle]
    static mut _IO_2_1_stdout_: _IO_FILE_plus;
    #[no_mangle]
    static mut _IO_2_1_stderr_: _IO_FILE_plus;
    #[no_mangle]
    static mut stdin: *mut _IO_FILE;
    #[no_mangle]
    static mut stdout: *mut _IO_FILE;
    #[no_mangle]
    static mut stderr: *mut _IO_FILE;
    #[no_mangle]
    static mut sys_nerr: libc::c_int;
    #[no_mangle]
    static sys_errlist: [*const libc::c_char; 0];
}
#[derive ( Copy , Clone )]
#[repr(C)]
pub struct te_expr {
    pub type_0: libc::c_int,
    pub unnamed: unnamed,
    pub parameters: [*mut libc::c_void; 1],
}
#[derive ( Copy , Clone )]
#[repr ( C )]
pub union unnamed {
    value: libc::c_double,
    bound: *const libc::c_double,
    function: *const libc::c_void,
}
pub type te_expr_0 = te_expr;
pub type unnamed_0 = libc::c_uint;
pub const TE_FLAG_PURE: unnamed_0 = 32;
pub const TE_CLOSURE7: unnamed_0 = 23;
pub const TE_CLOSURE6: unnamed_0 = 22;
pub const TE_CLOSURE5: unnamed_0 = 21;
pub const TE_CLOSURE4: unnamed_0 = 20;
pub const TE_CLOSURE3: unnamed_0 = 19;
pub const TE_CLOSURE2: unnamed_0 = 18;
pub const TE_CLOSURE1: unnamed_0 = 17;
pub const TE_CLOSURE0: unnamed_0 = 16;
pub const TE_FUNCTION7: unnamed_0 = 15;
pub const TE_FUNCTION6: unnamed_0 = 14;
pub const TE_FUNCTION5: unnamed_0 = 13;
pub const TE_FUNCTION4: unnamed_0 = 12;
pub const TE_FUNCTION3: unnamed_0 = 11;
pub const TE_FUNCTION2: unnamed_0 = 10;
pub const TE_FUNCTION1: unnamed_0 = 9;
pub const TE_FUNCTION0: unnamed_0 = 8;
pub const TE_VARIABLE: unnamed_0 = 0;
#[derive ( Copy , Clone )]
#[repr(C)]
pub struct te_variable {
    pub name: *const libc::c_char,
    pub address: *const libc::c_void,
    pub type_0: libc::c_int,
    pub context: *mut libc::c_void,
}
pub type te_variable_0 = te_variable;
pub type state = state_0;
#[derive ( Copy , Clone )]
#[repr(C)]
pub struct state_0 {
    pub start: *const libc::c_char,
    pub next: *const libc::c_char,
    pub type_0: libc::c_int,
    pub unnamed: unnamed_1,
    pub context: *mut libc::c_void,
    pub lookup: *const te_variable_0,
    pub lookup_len: libc::c_int,
}
#[derive ( Copy , Clone )]
#[repr ( C )]
pub union unnamed_1 {
    value: libc::c_double,
    bound: *const libc::c_double,
    function: *const libc::c_void,
}
pub const TOK_ERROR: unnamed_2 = 25;
pub const TOK_NULL: unnamed_2 = 24;
pub const TOK_SEP: unnamed_2 = 27;
pub const TOK_CLOSE: unnamed_2 = 29;
pub const TOK_OPEN: unnamed_2 = 28;
pub const TOK_INFIX: unnamed_2 = 32;
pub const TOK_VARIABLE: unnamed_2 = 31;
pub const TOK_NUMBER: unnamed_2 = 30;
pub const TOK_END: unnamed_2 = 26;
pub const TE_CONSTANT: unnamed_3 = 1;
/* COMPILE TIME OPTIONS */
/* Exponentiation associativity:
For a^b^c = (a^b)^c and -a^b = (-a)^b do nothing.
For a^b^c = a^(b^c) and -a^b = -(a^b) uncomment the next line.*/
/* #define TE_POW_FROM_RIGHT */
/* Logarithms
For log = base 10 log do nothing
For log = natural log uncomment the next line. */
/* #define TE_NAT_LOG */
pub type te_fun2 =
    Option<unsafe extern "C" fn(_: libc::c_double, _: libc::c_double)
               -> libc::c_double>;
pub type size_t = libc::c_ulong;
pub type __off_t = libc::c_long;
pub type __off64_t = libc::c_long;
pub const _SVID_: _LIB_VERSION_TYPE = 0;
pub type unnamed_2 = libc::c_uint;
pub const _POSIX_: _LIB_VERSION_TYPE = 2;
pub type _IO_lock_t = ();
pub const _IEEE_: _LIB_VERSION_TYPE = -1;
pub const _XOPEN_: _LIB_VERSION_TYPE = 1;
pub type unnamed_3 = libc::c_uint;
pub const _ISOC_: _LIB_VERSION_TYPE = 3;
pub type _LIB_VERSION_TYPE = libc::c_int;
#[derive ( Copy , Clone )]
#[repr(C)]
pub struct _IO_FILE {
    pub _flags: libc::c_int,
    pub _IO_read_ptr: *mut libc::c_char,
    pub _IO_read_end: *mut libc::c_char,
    pub _IO_read_base: *mut libc::c_char,
    pub _IO_write_base: *mut libc::c_char,
    pub _IO_write_ptr: *mut libc::c_char,
    pub _IO_write_end: *mut libc::c_char,
    pub _IO_buf_base: *mut libc::c_char,
    pub _IO_buf_end: *mut libc::c_char,
    pub _IO_save_base: *mut libc::c_char,
    pub _IO_backup_base: *mut libc::c_char,
    pub _IO_save_end: *mut libc::c_char,
    pub _markers: *mut _IO_marker,
    pub _chain: *mut _IO_FILE,
    pub _fileno: libc::c_int,
    pub _flags2: libc::c_int,
    pub _old_offset: __off_t,
    pub _cur_column: libc::c_ushort,
    pub _vtable_offset: libc::c_schar,
    pub _shortbuf: [libc::c_char; 1],
    pub _lock: *mut libc::c_void,
    pub _offset: __off64_t,
    pub __pad1: *mut libc::c_void,
    pub __pad2: *mut libc::c_void,
    pub __pad3: *mut libc::c_void,
    pub __pad4: *mut libc::c_void,
    pub __pad5: size_t,
    pub _mode: libc::c_int,
    pub _unused2: [libc::c_char; 20],
}
#[derive ( Copy , Clone )]
#[repr(C)]
pub struct _IO_marker {
    pub _next: *mut _IO_marker,
    pub _sbuf: *mut _IO_FILE,
    pub _pos: libc::c_int,
}
/* Parses the input expression, evaluates it, and frees it. */
/* Returns NaN on error. */
#[no_mangle]
pub unsafe extern "C" fn te_interp(mut expression: *const libc::c_char,
                                   mut error: *mut libc::c_int)
 -> libc::c_double {
    let mut n: *mut te_expr_0 =
        te_compile(expression, 0 as *const te_variable_0, 0i32, error);
    let mut ret: libc::c_double = 0.;
    if !n.is_null() {
        ret = te_eval(n);
        te_free(n);
    } else { ret = ::std::f32::NAN as libc::c_double }
    return ret;
}
/* Parses the input expression and binds variables. */
/* Returns NULL on error. */
#[no_mangle]
pub unsafe extern "C" fn te_compile(mut expression: *const libc::c_char,
                                    mut variables: *const te_variable_0,
                                    mut var_count: libc::c_int,
                                    mut error: *mut libc::c_int)
 -> *mut te_expr_0 {
    let mut s: state =
        state_0{start: 0 as *const libc::c_char,
                next: 0 as *const libc::c_char,
                type_0: 0,
                unnamed: unnamed_1{value: 0.,},
                context: 0 as *mut libc::c_void,
                lookup: 0 as *const te_variable_0,
                lookup_len: 0,};
    s.next = expression;
    s.start = s.next;
    s.lookup = variables;
    s.lookup_len = var_count;
    next_token(&mut s);
    let mut root: *mut te_expr_0 = list(&mut s);
    if s.type_0 != TOK_END as libc::c_int {
        te_free(root);
        if !error.is_null() {
            *error =
                s.start.offset_to(s.next).expect("bad offset_to") as
                    libc::c_long as libc::c_int;
            if *error == 0i32 { *error = 1i32 }
        }
        return 0 as *mut te_expr_0
    } else {
        optimize(root);
        if !error.is_null() { *error = 0i32 }
        return root
    };
}
unsafe extern "C" fn list(mut s: *mut state) -> *mut te_expr_0 {
    /* <list>      =    <expr> {"," <expr>} */
    let mut ret: *mut te_expr_0 = expr(s);
    while (*s).type_0 == TOK_SEP as libc::c_int {
        next_token(s);
        ret =
            new_expr(TE_FUNCTION2 as libc::c_int |
                         TE_FLAG_PURE as libc::c_int,
                     [ret, expr(s)].as_mut_ptr());
        (*ret).unnamed.function =
            ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                    libc::c_double,
                                                                _:
                                                                    libc::c_double)
                                               -> libc::c_double>,
                                    *const libc::c_void>(Some(comma))
    }
    return ret;
}
unsafe extern "C" fn expr(mut s: *mut state) -> *mut te_expr_0 {
    let mut t: te_fun2 = None;
    /* <expr>      =    <term> {("+" | "-") <term>} */
    let mut ret: *mut te_expr_0 = term(s);
    while (*s).type_0 == TOK_INFIX as libc::c_int &&
              ((*s).unnamed.function ==
                   ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                           libc::c_double,
                                                                       _:
                                                                           libc::c_double)
                                                      -> libc::c_double>,
                                           *const libc::c_void>(Some(add)) ||
                   (*s).unnamed.function ==
                       ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                               libc::c_double,
                                                                           _:
                                                                               libc::c_double)
                                                          -> libc::c_double>,
                                               *const libc::c_void>(Some(sub)))
          {
        t =
            ::std::mem::transmute::<*const libc::c_void,
                                    te_fun2>((*s).unnamed.function);
        next_token(s);
        ret =
            new_expr(TE_FUNCTION2 as libc::c_int |
                         TE_FLAG_PURE as libc::c_int,
                     [ret, term(s)].as_mut_ptr());
        (*ret).unnamed.function =
            ::std::mem::transmute::<te_fun2, *const libc::c_void>(t)
    }
    return ret;
}
unsafe extern "C" fn term(mut s: *mut state) -> *mut te_expr_0 {
    let mut t: te_fun2 = None;
    /* <term>      =    <factor> {("*" | "/" | "%") <factor>} */
    let mut ret: *mut te_expr_0 = factor(s);
    while (*s).type_0 == TOK_INFIX as libc::c_int &&
              ((*s).unnamed.function ==
                   ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                           libc::c_double,
                                                                       _:
                                                                           libc::c_double)
                                                      -> libc::c_double>,
                                           *const libc::c_void>(Some(mul)) ||
                   (*s).unnamed.function ==
                       ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                               libc::c_double,
                                                                           _:
                                                                               libc::c_double)
                                                          -> libc::c_double>,
                                               *const libc::c_void>(Some(divide))
                   ||
                   (*s).unnamed.function ==
                       ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                               libc::c_double,
                                                                           _:
                                                                               libc::c_double)
                                                          -> libc::c_double>,
                                               *const libc::c_void>(Some(fmod)))
          {
        t =
            ::std::mem::transmute::<*const libc::c_void,
                                    te_fun2>((*s).unnamed.function);
        next_token(s);
        ret =
            new_expr(TE_FUNCTION2 as libc::c_int |
                         TE_FLAG_PURE as libc::c_int,
                     [ret, factor(s)].as_mut_ptr());
        (*ret).unnamed.function =
            ::std::mem::transmute::<te_fun2, *const libc::c_void>(t)
    }
    return ret;
}
unsafe extern "C" fn factor(mut s: *mut state) -> *mut te_expr_0 {
    let mut t: te_fun2 = None;
    /* <factor>    =    <power> {"^" <power>} */
    let mut ret: *mut te_expr_0 = power(s);
    while (*s).type_0 == TOK_INFIX as libc::c_int &&
              (*s).unnamed.function ==
                  ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                          libc::c_double,
                                                                      _:
                                                                          libc::c_double)
                                                     -> libc::c_double>,
                                          *const libc::c_void>(Some(pow)) {
        t =
            ::std::mem::transmute::<*const libc::c_void,
                                    te_fun2>((*s).unnamed.function);
        next_token(s);
        ret =
            new_expr(TE_FUNCTION2 as libc::c_int |
                         TE_FLAG_PURE as libc::c_int,
                     [ret, power(s)].as_mut_ptr());
        (*ret).unnamed.function =
            ::std::mem::transmute::<te_fun2, *const libc::c_void>(t)
    }
    return ret;
}
unsafe extern "C" fn power(mut s: *mut state) -> *mut te_expr_0 {
    /* <power>     =    {("-" | "+")} <base> */
    let mut sign: libc::c_int = 1i32;
    while (*s).type_0 == TOK_INFIX as libc::c_int &&
              ((*s).unnamed.function ==
                   ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                           libc::c_double,
                                                                       _:
                                                                           libc::c_double)
                                                      -> libc::c_double>,
                                           *const libc::c_void>(Some(add)) ||
                   (*s).unnamed.function ==
                       ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                               libc::c_double,
                                                                           _:
                                                                               libc::c_double)
                                                          -> libc::c_double>,
                                               *const libc::c_void>(Some(sub)))
          {
        if (*s).unnamed.function ==
               ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                       libc::c_double,
                                                                   _:
                                                                       libc::c_double)
                                                  -> libc::c_double>,
                                       *const libc::c_void>(Some(sub)) {
            sign = -sign
        }
        next_token(s);
    }
    let mut ret: *mut te_expr_0 = 0 as *mut te_expr_0;
    if sign == 1i32 {
        ret = base(s)
    } else {
        ret =
            new_expr(TE_FUNCTION1 as libc::c_int |
                         TE_FLAG_PURE as libc::c_int, [base(s)].as_mut_ptr());
        (*ret).unnamed.function =
            ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                    libc::c_double)
                                               -> libc::c_double>,
                                    *const libc::c_void>(Some(negate))
    }
    return ret;
}
unsafe extern "C" fn negate(mut a: libc::c_double) -> libc::c_double {
    return -a;
}
unsafe extern "C" fn base(mut s: *mut state) -> *mut te_expr_0 {
    /* <base>      =    <constant> | <variable> | <function-0> {"(" ")"} | <function-1> <power> | <function-X> "(" <expr> {"," <expr>} ")" | "(" <list> ")" */
    let mut ret: *mut te_expr_0 = 0 as *mut te_expr_0;
    let mut arity: libc::c_int = 0;
    match (*s).type_0 & 31i32 {
        30 => {
            ret =
                new_expr(TE_CONSTANT as libc::c_int,
                         0 as *mut *const te_expr_0);
            (*ret).unnamed.value = (*s).unnamed.value;
            next_token(s);
        }
        31 => {
            ret =
                new_expr(TE_VARIABLE as libc::c_int,
                         0 as *mut *const te_expr_0);
            (*ret).unnamed.bound = (*s).unnamed.bound;
            next_token(s);
        }
        8 | 16 => {
            ret = new_expr((*s).type_0, 0 as *mut *const te_expr_0);
            (*ret).unnamed.function = (*s).unnamed.function;
            if (*s).type_0 & TE_CLOSURE0 as libc::c_int != 0i32 {
                (*ret).parameters[0usize] = (*s).context
            }
            next_token(s);
            if (*s).type_0 == TOK_OPEN as libc::c_int {
                next_token(s);
                if (*s).type_0 != TOK_CLOSE as libc::c_int {
                    (*s).type_0 = TOK_ERROR as libc::c_int
                } else { next_token(s); }
            }
        }
        9 | 17 => {
            ret = new_expr((*s).type_0, 0 as *mut *const te_expr_0);
            (*ret).unnamed.function = (*s).unnamed.function;
            if (*s).type_0 & TE_CLOSURE0 as libc::c_int != 0i32 {
                (*ret).parameters[1usize] = (*s).context
            }
            next_token(s);
            (*ret).parameters[0usize] = power(s) as *mut libc::c_void
        }
        10 | 11 | 12 | 13 | 14 | 15 | 18 | 19 | 20 | 21 | 22 | 23 => {
            arity =
                if 0 !=
                       (*s).type_0 &
                           (TE_FUNCTION0 as libc::c_int |
                                TE_CLOSURE0 as libc::c_int) {
                    (*s).type_0 & 7i32
                } else { 0i32 };
            ret = new_expr((*s).type_0, 0 as *mut *const te_expr_0);
            (*ret).unnamed.function = (*s).unnamed.function;
            if (*s).type_0 & TE_CLOSURE0 as libc::c_int != 0i32 {
                (*ret).parameters[arity as usize] = (*s).context
            }
            next_token(s);
            if (*s).type_0 != TOK_OPEN as libc::c_int {
                (*s).type_0 = TOK_ERROR as libc::c_int
            } else {
                let mut i: libc::c_int = 0;
                i = 0i32;
                while i < arity {
                    next_token(s);
                    (*ret).parameters[i as usize] =
                        expr(s) as *mut libc::c_void;
                    if (*s).type_0 != TOK_SEP as libc::c_int { break ; }
                    i += 1
                }
                if (*s).type_0 != TOK_CLOSE as libc::c_int ||
                       i != arity - 1i32 {
                    (*s).type_0 = TOK_ERROR as libc::c_int
                } else { next_token(s); }
            }
        }
        28 => {
            next_token(s);
            ret = list(s);
            if (*s).type_0 != TOK_CLOSE as libc::c_int {
                (*s).type_0 = TOK_ERROR as libc::c_int
            } else { next_token(s); }
        }
        _ => {
            ret = new_expr(0i32, 0 as *mut *const te_expr_0);
            (*s).type_0 = TOK_ERROR as libc::c_int;
            (*ret).unnamed.value = ::std::f32::NAN as libc::c_double
        }
    }
    return ret;
}
unsafe extern "C" fn new_expr(type_0: libc::c_int,
                              mut parameters: *mut *const te_expr_0)
 -> *mut te_expr_0 {
    let arity: libc::c_int =
        if 0 !=
               type_0 &
                   (TE_FUNCTION0 as libc::c_int | TE_CLOSURE0 as libc::c_int)
           {
            type_0 & 7i32
        } else { 0i32 };
    let psize: libc::c_int =
        (::std::mem::size_of::<*mut libc::c_void>() as
             libc::c_ulong).wrapping_mul(arity as libc::c_ulong) as
            libc::c_int;
    let size: libc::c_int =
        (::std::mem::size_of::<te_expr_0>() as
             libc::c_ulong).wrapping_sub(::std::mem::size_of::<*mut libc::c_void>()
                                             as
                                             libc::c_ulong).wrapping_add(psize
                                                                             as
                                                                             libc::c_ulong).wrapping_add(if type_0
                                                                                                                &
                                                                                                                TE_CLOSURE0
                                                                                                                    as
                                                                                                                    libc::c_int
                                                                                                                !=
                                                                                                                0i32
                                                                                                            {
                                                                                                             ::std::mem::size_of::<*mut libc::c_void>()
                                                                                                                 as
                                                                                                                 libc::c_ulong
                                                                                                         } else {
                                                                                                             0i32
                                                                                                                 as
                                                                                                                 libc::c_ulong
                                                                                                         })
            as libc::c_int;
    let mut ret: *mut te_expr_0 =
        malloc(size as libc::c_ulong) as *mut te_expr_0;
    memset(ret as *mut libc::c_void, 0i32, size as libc::c_ulong);
    if 0 != arity && !parameters.is_null() {
        memcpy((*ret).parameters.as_mut_ptr() as *mut libc::c_void,
               parameters as *const libc::c_void, psize as libc::c_ulong);
    }
    (*ret).type_0 = type_0;
    (*ret).unnamed.bound = 0 as *const libc::c_double;
    return ret;
}
#[no_mangle]
pub unsafe extern "C" fn next_token(mut s: *mut state) -> () {
    let mut current_block: u64;
    (*s).type_0 = TOK_NULL as libc::c_int;
    loop  {
        if 0 == *(*s).next {
            (*s).type_0 = TOK_END as libc::c_int;
            return
        } else {
            /* Try reading a number. */
            if *(*s).next.offset(0isize) as libc::c_int >= '0' as i32 &&
                   *(*s).next.offset(0isize) as libc::c_int <= '9' as i32 ||
                   *(*s).next.offset(0isize) as libc::c_int == '.' as i32 {
                (*s).unnamed.value =
                    strtod((*s).next,
                           &mut (*s).next as *mut *const libc::c_char as
                               *mut *mut libc::c_char);
                (*s).type_0 = TOK_NUMBER as libc::c_int
            } else if *(*s).next.offset(0isize) as libc::c_int >= 'a' as i32
                          &&
                          *(*s).next.offset(0isize) as libc::c_int <=
                              'z' as i32 {
                let mut start: *const libc::c_char = 0 as *const libc::c_char;
                start = (*s).next;
                while *(*s).next.offset(0isize) as libc::c_int >= 'a' as i32
                          &&
                          *(*s).next.offset(0isize) as libc::c_int <=
                              'z' as i32 ||
                          *(*s).next.offset(0isize) as libc::c_int >=
                              '0' as i32 &&
                              *(*s).next.offset(0isize) as libc::c_int <=
                                  '9' as i32 ||
                          *(*s).next.offset(0isize) as libc::c_int ==
                              '_' as i32 {
                    (*s).next = (*s).next.offset(1isize)
                }
                let mut var: *const te_variable_0 =
                    find_lookup(s, start,
                                (start as *const BYTE).offset_to((*s).next as *const libc::uint8_t).expect("bad offset_to")
                                    as libc::c_long as libc::c_int);
                if var.is_null() {
                    var =
                        find_builtin(start,
                                    ( start as *const  BYTE).offset_to((*s).next).expect("bad offset_to")
                                         as libc::c_long as libc::c_int)
                }
                if var.is_null() {
                    (*s).type_0 = TOK_ERROR as libc::c_int
                } else {
                    match (*var).type_0 & 31i32 {
                        0 => {
                            current_block = 535428906686936532;
                            match current_block {
                                5773172663599036497 => {
                                    /* Falls through. */
                                    current_block = 12176935636353528326;
                                }
                                535428906686936532 => {
                                    (*s).type_0 = TOK_VARIABLE as libc::c_int;
                                    (*s).unnamed.bound =
                                        (*var).address as
                                            *const libc::c_double;
                                    current_block = 9386390421034826751;
                                }
                                _ => { }
                            }
                            match current_block {
                                9386390421034826751 => { }
                                _ => {
                                    match current_block {
                                        12176935636353528326 => {
                                            /* Falls through. */
                                            current_block =
                                                4283923591314305871;
                                        }
                                        _ => { }
                                    }
                                    match current_block {
                                        4283923591314305871 => {
                                            /* Falls through. */
                                            (*s).context = (*var).context;
                                            current_block =
                                                14016947558782682488;
                                        }
                                        _ => { }
                                    }
                                    match current_block {
                                        14016947558782682488 => {
                                            /* Falls through. */
                                            current_block =
                                                11394448613837997297;
                                        }
                                        _ => { }
                                    }
                                    match current_block {
                                        11394448613837997297 => { }
                                        _ => { }
                                    }
                                    /* Falls through. */
                                    (*s).type_0 = (*var).type_0;
                                    (*s).unnamed.function = (*var).address
                                }
                            }
                        }
                        16 | 17 | 18 => {
                            current_block = 5773172663599036497;
                            match current_block {
                                5773172663599036497 => {
                                    /* Falls through. */
                                    current_block = 12176935636353528326;
                                }
                                535428906686936532 => {
                                    (*s).type_0 = TOK_VARIABLE as libc::c_int;
                                    (*s).unnamed.bound =
                                        (*var).address as
                                            *const libc::c_double;
                                    current_block = 9386390421034826751;
                                }
                                _ => { }
                            }
                            match current_block {
                                9386390421034826751 => { }
                                _ => {
                                    match current_block {
                                        12176935636353528326 => {
                                            /* Falls through. */
                                            current_block =
                                                4283923591314305871;
                                        }
                                        _ => { }
                                    }
                                    match current_block {
                                        4283923591314305871 => {
                                            /* Falls through. */
                                            (*s).context = (*var).context;
                                            current_block =
                                                14016947558782682488;
                                        }
                                        _ => { }
                                    }
                                    match current_block {
                                        14016947558782682488 => {
                                            /* Falls through. */
                                            current_block =
                                                11394448613837997297;
                                        }
                                        _ => { }
                                    }
                                    match current_block {
                                        11394448613837997297 => { }
                                        _ => { }
                                    }
                                    /* Falls through. */
                                    (*s).type_0 = (*var).type_0;
                                    (*s).unnamed.function = (*var).address
                                }
                            }
                        }
                        19 | 20 | 21 | 22 => {
                            current_block = 12176935636353528326;
                            match current_block {
                                5773172663599036497 => {
                                    /* Falls through. */
                                    current_block = 12176935636353528326;
                                }
                                535428906686936532 => {
                                    (*s).type_0 = TOK_VARIABLE as libc::c_int;
                                    (*s).unnamed.bound =
                                        (*var).address as
                                            *const libc::c_double;
                                    current_block = 9386390421034826751;
                                }
                                _ => { }
                            }
                            match current_block {
                                9386390421034826751 => { }
                                _ => {
                                    match current_block {
                                        12176935636353528326 => {
                                            /* Falls through. */
                                            current_block =
                                                4283923591314305871;
                                        }
                                        _ => { }
                                    }
                                    match current_block {
                                        4283923591314305871 => {
                                            /* Falls through. */
                                            (*s).context = (*var).context;
                                            current_block =
                                                14016947558782682488;
                                        }
                                        _ => { }
                                    }
                                    match current_block {
                                        14016947558782682488 => {
                                            /* Falls through. */
                                            current_block =
                                                11394448613837997297;
                                        }
                                        _ => { }
                                    }
                                    match current_block {
                                        11394448613837997297 => { }
                                        _ => { }
                                    }
                                    /* Falls through. */
                                    (*s).type_0 = (*var).type_0;
                                    (*s).unnamed.function = (*var).address
                                }
                            }
                        }
                        23 => {
                            current_block = 4283923591314305871;
                            match current_block {
                                5773172663599036497 => {
                                    /* Falls through. */
                                    current_block = 12176935636353528326;
                                }
                                535428906686936532 => {
                                    (*s).type_0 = TOK_VARIABLE as libc::c_int;
                                    (*s).unnamed.bound =
                                        (*var).address as
                                            *const libc::c_double;
                                    current_block = 9386390421034826751;
                                }
                                _ => { }
                            }
                            match current_block {
                                9386390421034826751 => { }
                                _ => {
                                    match current_block {
                                        12176935636353528326 => {
                                            /* Falls through. */
                                            current_block =
                                                4283923591314305871;
                                        }
                                        _ => { }
                                    }
                                    match current_block {
                                        4283923591314305871 => {
                                            /* Falls through. */
                                            (*s).context = (*var).context;
                                            current_block =
                                                14016947558782682488;
                                        }
                                        _ => { }
                                    }
                                    match current_block {
                                        14016947558782682488 => {
                                            /* Falls through. */
                                            current_block =
                                                11394448613837997297;
                                        }
                                        _ => { }
                                    }
                                    match current_block {
                                        11394448613837997297 => { }
                                        _ => { }
                                    }
                                    /* Falls through. */
                                    (*s).type_0 = (*var).type_0;
                                    (*s).unnamed.function = (*var).address
                                }
                            }
                        }
                        8 | 9 | 10 => {
                            current_block = 14016947558782682488;
                            match current_block {
                                5773172663599036497 => {
                                    /* Falls through. */
                                    current_block = 12176935636353528326;
                                }
                                535428906686936532 => {
                                    (*s).type_0 = TOK_VARIABLE as libc::c_int;
                                    (*s).unnamed.bound =
                                        (*var).address as
                                            *const libc::c_double;
                                    current_block = 9386390421034826751;
                                }
                                _ => { }
                            }
                            match current_block {
                                9386390421034826751 => { }
                                _ => {
                                    match current_block {
                                        12176935636353528326 => {
                                            /* Falls through. */
                                            current_block =
                                                4283923591314305871;
                                        }
                                        _ => { }
                                    }
                                    match current_block {
                                        4283923591314305871 => {
                                            /* Falls through. */
                                            (*s).context = (*var).context;
                                            current_block =
                                                14016947558782682488;
                                        }
                                        _ => { }
                                    }
                                    match current_block {
                                        14016947558782682488 => {
                                            /* Falls through. */
                                            current_block =
                                                11394448613837997297;
                                        }
                                        _ => { }
                                    }
                                    match current_block {
                                        11394448613837997297 => { }
                                        _ => { }
                                    }
                                    /* Falls through. */
                                    (*s).type_0 = (*var).type_0;
                                    (*s).unnamed.function = (*var).address
                                }
                            }
                        }
                        11 | 12 | 13 | 14 => {
                            current_block = 11394448613837997297;
                            match current_block {
                                5773172663599036497 => {
                                    /* Falls through. */
                                    current_block = 12176935636353528326;
                                }
                                535428906686936532 => {
                                    (*s).type_0 = TOK_VARIABLE as libc::c_int;
                                    (*s).unnamed.bound =
                                        (*var).address as
                                            *const libc::c_double;
                                    current_block = 9386390421034826751;
                                }
                                _ => { }
                            }
                            match current_block {
                                9386390421034826751 => { }
                                _ => {
                                    match current_block {
                                        12176935636353528326 => {
                                            /* Falls through. */
                                            current_block =
                                                4283923591314305871;
                                        }
                                        _ => { }
                                    }
                                    match current_block {
                                        4283923591314305871 => {
                                            /* Falls through. */
                                            (*s).context = (*var).context;
                                            current_block =
                                                14016947558782682488;
                                        }
                                        _ => { }
                                    }
                                    match current_block {
                                        14016947558782682488 => {
                                            /* Falls through. */
                                            current_block =
                                                11394448613837997297;
                                        }
                                        _ => { }
                                    }
                                    match current_block {
                                        11394448613837997297 => { }
                                        _ => { }
                                    }
                                    /* Falls through. */
                                    (*s).type_0 = (*var).type_0;
                                    (*s).unnamed.function = (*var).address
                                }
                            }
                        }
                        15 => {
                            current_block = 3728098941540484526;
                            match current_block {
                                5773172663599036497 => {
                                    /* Falls through. */
                                    current_block = 12176935636353528326;
                                }
                                535428906686936532 => {
                                    (*s).type_0 = TOK_VARIABLE as libc::c_int;
                                    (*s).unnamed.bound =
                                        (*var).address as
                                            *const libc::c_double;
                                    current_block = 9386390421034826751;
                                }
                                _ => { }
                            }
                            match current_block {
                                9386390421034826751 => { }
                                _ => {
                                    match current_block {
                                        12176935636353528326 => {
                                            /* Falls through. */
                                            current_block =
                                                4283923591314305871;
                                        }
                                        _ => { }
                                    }
                                    match current_block {
                                        4283923591314305871 => {
                                            /* Falls through. */
                                            (*s).context = (*var).context;
                                            current_block =
                                                14016947558782682488;
                                        }
                                        _ => { }
                                    }
                                    match current_block {
                                        14016947558782682488 => {
                                            /* Falls through. */
                                            current_block =
                                                11394448613837997297;
                                        }
                                        _ => { }
                                    }
                                    match current_block {
                                        11394448613837997297 => { }
                                        _ => { }
                                    }
                                    /* Falls through. */
                                    (*s).type_0 = (*var).type_0;
                                    (*s).unnamed.function = (*var).address
                                }
                            }
                        }
                        _ => { }
                    }
                }
            } else {
                /* Look for an operator or special character. */
                let fresh0 = (*s).next;
                (*s).next = (*s).next.offset(1);
                match *fresh0.offset(0isize) as libc::c_int {
                    43 => {
                        current_block = 5573483010270415326;
                        match current_block {
                            7685787670966732963 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(sub))
                            }
                            5573483010270415326 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(add))
                            }
                            12012411429364623702 => {
                                (*s).type_0 = TOK_OPEN as libc::c_int
                            }
                            8052470741125375710 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(mul))
                            }
                            3065446020167570027 => {
                                (*s).type_0 = TOK_SEP as libc::c_int
                            }
                            8425206765891123226 => {
                                (*s).type_0 = TOK_CLOSE as libc::c_int
                            }
                            7394781936252865942 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(divide))
                            }
                            998291431524734847 => {
                                (*s).type_0 = TOK_ERROR as libc::c_int
                            }
                            14965538305503626156 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(fmod))
                            }
                            _ => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(pow))
                            }
                        }
                    }
                    45 => {
                        current_block = 7685787670966732963;
                        match current_block {
                            7685787670966732963 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(sub))
                            }
                            5573483010270415326 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(add))
                            }
                            12012411429364623702 => {
                                (*s).type_0 = TOK_OPEN as libc::c_int
                            }
                            8052470741125375710 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(mul))
                            }
                            3065446020167570027 => {
                                (*s).type_0 = TOK_SEP as libc::c_int
                            }
                            8425206765891123226 => {
                                (*s).type_0 = TOK_CLOSE as libc::c_int
                            }
                            7394781936252865942 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(divide))
                            }
                            998291431524734847 => {
                                (*s).type_0 = TOK_ERROR as libc::c_int
                            }
                            14965538305503626156 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(fmod))
                            }
                            _ => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(pow))
                            }
                        }
                    }
                    42 => {
                        current_block = 8052470741125375710;
                        match current_block {
                            7685787670966732963 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(sub))
                            }
                            5573483010270415326 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(add))
                            }
                            12012411429364623702 => {
                                (*s).type_0 = TOK_OPEN as libc::c_int
                            }
                            8052470741125375710 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(mul))
                            }
                            3065446020167570027 => {
                                (*s).type_0 = TOK_SEP as libc::c_int
                            }
                            8425206765891123226 => {
                                (*s).type_0 = TOK_CLOSE as libc::c_int
                            }
                            7394781936252865942 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(divide))
                            }
                            998291431524734847 => {
                                (*s).type_0 = TOK_ERROR as libc::c_int
                            }
                            14965538305503626156 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(fmod))
                            }
                            _ => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(pow))
                            }
                        }
                    }
                    47 => {
                        current_block = 7394781936252865942;
                        match current_block {
                            7685787670966732963 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(sub))
                            }
                            5573483010270415326 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(add))
                            }
                            12012411429364623702 => {
                                (*s).type_0 = TOK_OPEN as libc::c_int
                            }
                            8052470741125375710 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(mul))
                            }
                            3065446020167570027 => {
                                (*s).type_0 = TOK_SEP as libc::c_int
                            }
                            8425206765891123226 => {
                                (*s).type_0 = TOK_CLOSE as libc::c_int
                            }
                            7394781936252865942 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(divide))
                            }
                            998291431524734847 => {
                                (*s).type_0 = TOK_ERROR as libc::c_int
                            }
                            14965538305503626156 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(fmod))
                            }
                            _ => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(pow))
                            }
                        }
                    }
                    94 => {
                        current_block = 11887652536949061783;
                        match current_block {
                            7685787670966732963 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(sub))
                            }
                            5573483010270415326 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(add))
                            }
                            12012411429364623702 => {
                                (*s).type_0 = TOK_OPEN as libc::c_int
                            }
                            8052470741125375710 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(mul))
                            }
                            3065446020167570027 => {
                                (*s).type_0 = TOK_SEP as libc::c_int
                            }
                            8425206765891123226 => {
                                (*s).type_0 = TOK_CLOSE as libc::c_int
                            }
                            7394781936252865942 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(divide))
                            }
                            998291431524734847 => {
                                (*s).type_0 = TOK_ERROR as libc::c_int
                            }
                            14965538305503626156 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(fmod))
                            }
                            _ => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(pow))
                            }
                        }
                    }
                    37 => {
                        current_block = 14965538305503626156;
                        match current_block {
                            7685787670966732963 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(sub))
                            }
                            5573483010270415326 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(add))
                            }
                            12012411429364623702 => {
                                (*s).type_0 = TOK_OPEN as libc::c_int
                            }
                            8052470741125375710 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(mul))
                            }
                            3065446020167570027 => {
                                (*s).type_0 = TOK_SEP as libc::c_int
                            }
                            8425206765891123226 => {
                                (*s).type_0 = TOK_CLOSE as libc::c_int
                            }
                            7394781936252865942 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(divide))
                            }
                            998291431524734847 => {
                                (*s).type_0 = TOK_ERROR as libc::c_int
                            }
                            14965538305503626156 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(fmod))
                            }
                            _ => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(pow))
                            }
                        }
                    }
                    40 => {
                        current_block = 12012411429364623702;
                        match current_block {
                            7685787670966732963 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(sub))
                            }
                            5573483010270415326 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(add))
                            }
                            12012411429364623702 => {
                                (*s).type_0 = TOK_OPEN as libc::c_int
                            }
                            8052470741125375710 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(mul))
                            }
                            3065446020167570027 => {
                                (*s).type_0 = TOK_SEP as libc::c_int
                            }
                            8425206765891123226 => {
                                (*s).type_0 = TOK_CLOSE as libc::c_int
                            }
                            7394781936252865942 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(divide))
                            }
                            998291431524734847 => {
                                (*s).type_0 = TOK_ERROR as libc::c_int
                            }
                            14965538305503626156 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(fmod))
                            }
                            _ => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(pow))
                            }
                        }
                    }
                    41 => {
                        current_block = 8425206765891123226;
                        match current_block {
                            7685787670966732963 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(sub))
                            }
                            5573483010270415326 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(add))
                            }
                            12012411429364623702 => {
                                (*s).type_0 = TOK_OPEN as libc::c_int
                            }
                            8052470741125375710 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(mul))
                            }
                            3065446020167570027 => {
                                (*s).type_0 = TOK_SEP as libc::c_int
                            }
                            8425206765891123226 => {
                                (*s).type_0 = TOK_CLOSE as libc::c_int
                            }
                            7394781936252865942 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(divide))
                            }
                            998291431524734847 => {
                                (*s).type_0 = TOK_ERROR as libc::c_int
                            }
                            14965538305503626156 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(fmod))
                            }
                            _ => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(pow))
                            }
                        }
                    }
                    44 => {
                        current_block = 3065446020167570027;
                        match current_block {
                            7685787670966732963 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(sub))
                            }
                            5573483010270415326 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(add))
                            }
                            12012411429364623702 => {
                                (*s).type_0 = TOK_OPEN as libc::c_int
                            }
                            8052470741125375710 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(mul))
                            }
                            3065446020167570027 => {
                                (*s).type_0 = TOK_SEP as libc::c_int
                            }
                            8425206765891123226 => {
                                (*s).type_0 = TOK_CLOSE as libc::c_int
                            }
                            7394781936252865942 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(divide))
                            }
                            998291431524734847 => {
                                (*s).type_0 = TOK_ERROR as libc::c_int
                            }
                            14965538305503626156 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(fmod))
                            }
                            _ => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(pow))
                            }
                        }
                    }
                    32 | 9 | 10 | 13 => { }
                    _ => {
                        current_block = 998291431524734847;
                        match current_block {
                            7685787670966732963 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(sub))
                            }
                            5573483010270415326 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(add))
                            }
                            12012411429364623702 => {
                                (*s).type_0 = TOK_OPEN as libc::c_int
                            }
                            8052470741125375710 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(mul))
                            }
                            3065446020167570027 => {
                                (*s).type_0 = TOK_SEP as libc::c_int
                            }
                            8425206765891123226 => {
                                (*s).type_0 = TOK_CLOSE as libc::c_int
                            }
                            7394781936252865942 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(divide))
                            }
                            998291431524734847 => {
                                (*s).type_0 = TOK_ERROR as libc::c_int
                            }
                            14965538305503626156 => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(fmod))
                            }
                            _ => {
                                (*s).type_0 = TOK_INFIX as libc::c_int;
                                (*s).unnamed.function =
                                    ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                            libc::c_double,
                                                                                        _:
                                                                                            libc::c_double)
                                                                       ->
                                                                           libc::c_double>,
                                                            *const libc::c_void>(Some(pow))
                            }
                        }
                    }
                }
            }
            if !((*s).type_0 == TOK_NULL as libc::c_int) { break ; }
        }
    };
}
unsafe extern "C" fn divide(mut a: libc::c_double, mut b: libc::c_double)
 -> libc::c_double {
    return a / b;
}
unsafe extern "C" fn mul(mut a: libc::c_double, mut b: libc::c_double)
 -> libc::c_double {
    return a * b;
}
unsafe extern "C" fn sub(mut a: libc::c_double, mut b: libc::c_double)
 -> libc::c_double {
    return a - b;
}
unsafe extern "C" fn add(mut a: libc::c_double, mut b: libc::c_double)
 -> libc::c_double {
    return a + b;
}
unsafe extern "C" fn find_lookup(mut s: *const state,
                                 mut name: *const libc::c_char,
                                 mut len: libc::c_int)
 -> *const te_variable_0 {
    let mut iters: libc::c_int = 0;
    let mut var: *const te_variable_0 = 0 as *const te_variable_0;
    if (*s).lookup.is_null() {
        return 0 as *const te_variable_0
    } else {
        var = (*s).lookup;
        iters = (*s).lookup_len;
        while 0 != iters {
            if strncmp(name, (*var).name, len as libc::c_ulong) == 0i32 &&
                   *(*var).name.offset(len as isize) as libc::c_int ==
                       '\u{0}' as i32 {
                return var
            } else { var = var.offset(1isize); iters -= 1 }
        }
        return 0 as *const te_variable_0
    };
}
/* must be in alphabetical order */
unsafe extern "C" fn find_builtin(mut name: *const libc::c_char,
                                  mut len: libc::c_int)
 -> *const te_variable_0 {
    let mut imin: libc::c_int = 0i32;
    let mut imax: libc::c_int =
        (::std::mem::size_of::<[te_variable_0; 25]>() as
             libc::c_ulong).wrapping_div(::std::mem::size_of::<te_variable_0>()
                                             as
                                             libc::c_ulong).wrapping_sub(2i32
                                                                             as
                                                                             libc::c_ulong)
            as libc::c_int;
    /*Binary search.*/
    while imax >= imin {
        let i: libc::c_int = imin + (imax - imin) / 2i32;
        let mut c: libc::c_int =
            strncmp(name, functions[i as usize].name, len as libc::c_ulong);
        if 0 == c {
            c =
                '\u{0}' as i32 -
                    *functions[i as usize].name.offset(len as isize) as
                        libc::c_int
        }
        if c == 0i32 {
            return functions.as_ptr().offset(i as isize)
        } else if c > 0i32 { imin = i + 1i32 } else { imax = i - 1i32 }
    }
    return 0 as *const te_variable_0;
}
static mut functions: [te_variable_0; 25] =
    unsafe {
        [te_variable{name: b"abs\x00" as *const u8 as *const libc::c_char,
                     address:
                         ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                 libc::c_double)
                                                            ->
                                                                libc::c_double>,
                                                 *const libc::c_void>(Some(fabs)),
                     type_0:
                         TE_FUNCTION1 as libc::c_int |
                             TE_FLAG_PURE as libc::c_int,
                     context: 0 as *const libc::c_void as *mut libc::c_void,},
         te_variable{name: b"acos\x00" as *const u8 as *const libc::c_char,
                     address:
                         ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                 libc::c_double)
                                                            ->
                                                                libc::c_double>,
                                                 *const libc::c_void>(Some(acos)),
                     type_0:
                         TE_FUNCTION1 as libc::c_int |
                             TE_FLAG_PURE as libc::c_int,
                     context: 0 as *const libc::c_void as *mut libc::c_void,},
         te_variable{name: b"asin\x00" as *const u8 as *const libc::c_char,
                     address:
                         ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                 libc::c_double)
                                                            ->
                                                                libc::c_double>,
                                                 *const libc::c_void>(Some(asin)),
                     type_0:
                         TE_FUNCTION1 as libc::c_int |
                             TE_FLAG_PURE as libc::c_int,
                     context: 0 as *const libc::c_void as *mut libc::c_void,},
         te_variable{name: b"atan\x00" as *const u8 as *const libc::c_char,
                     address:
                         ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                 libc::c_double)
                                                            ->
                                                                libc::c_double>,
                                                 *const libc::c_void>(Some(atan)),
                     type_0:
                         TE_FUNCTION1 as libc::c_int |
                             TE_FLAG_PURE as libc::c_int,
                     context: 0 as *const libc::c_void as *mut libc::c_void,},
         te_variable{name: b"atan2\x00" as *const u8 as *const libc::c_char,
                     address:
                         ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                 libc::c_double,
                                                                             _:
                                                                                 libc::c_double)
                                                            ->
                                                                libc::c_double>,
                                                 *const libc::c_void>(Some(atan2)),
                     type_0:
                         TE_FUNCTION2 as libc::c_int |
                             TE_FLAG_PURE as libc::c_int,
                     context: 0 as *const libc::c_void as *mut libc::c_void,},
         te_variable{name: b"ceil\x00" as *const u8 as *const libc::c_char,
                     address:
                         ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                 libc::c_double)
                                                            ->
                                                                libc::c_double>,
                                                 *const libc::c_void>(Some(ceil)),
                     type_0:
                         TE_FUNCTION1 as libc::c_int |
                             TE_FLAG_PURE as libc::c_int,
                     context: 0 as *const libc::c_void as *mut libc::c_void,},
         te_variable{name: b"cos\x00" as *const u8 as *const libc::c_char,
                     address:
                         ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                 libc::c_double)
                                                            ->
                                                                libc::c_double>,
                                                 *const libc::c_void>(Some(cos)),
                     type_0:
                         TE_FUNCTION1 as libc::c_int |
                             TE_FLAG_PURE as libc::c_int,
                     context: 0 as *const libc::c_void as *mut libc::c_void,},
         te_variable{name: b"cosh\x00" as *const u8 as *const libc::c_char,
                     address:
                         ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                 libc::c_double)
                                                            ->
                                                                libc::c_double>,
                                                 *const libc::c_void>(Some(cosh)),
                     type_0:
                         TE_FUNCTION1 as libc::c_int |
                             TE_FLAG_PURE as libc::c_int,
                     context: 0 as *const libc::c_void as *mut libc::c_void,},
         te_variable{name: b"e\x00" as *const u8 as *const libc::c_char,
                     address:
                         ::std::mem::transmute::<Option<unsafe extern "C" fn()
                                                            ->
                                                                libc::c_double>,
                                                 *const libc::c_void>(Some(e)),
                     type_0:
                         TE_FUNCTION0 as libc::c_int |
                             TE_FLAG_PURE as libc::c_int,
                     context: 0 as *const libc::c_void as *mut libc::c_void,},
         te_variable{name: b"exp\x00" as *const u8 as *const libc::c_char,
                     address:
                         ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                 libc::c_double)
                                                            ->
                                                                libc::c_double>,
                                                 *const libc::c_void>(Some(exp)),
                     type_0:
                         TE_FUNCTION1 as libc::c_int |
                             TE_FLAG_PURE as libc::c_int,
                     context: 0 as *const libc::c_void as *mut libc::c_void,},
         te_variable{name: b"fac\x00" as *const u8 as *const libc::c_char,
                     address:
                         ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                 libc::c_double)
                                                            ->
                                                                libc::c_double>,
                                                 *const libc::c_void>(Some(fac)),
                     type_0:
                         TE_FUNCTION1 as libc::c_int |
                             TE_FLAG_PURE as libc::c_int,
                     context: 0 as *const libc::c_void as *mut libc::c_void,},
         te_variable{name: b"floor\x00" as *const u8 as *const libc::c_char,
                     address:
                         ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                 libc::c_double)
                                                            ->
                                                                libc::c_double>,
                                                 *const libc::c_void>(Some(floor)),
                     type_0:
                         TE_FUNCTION1 as libc::c_int |
                             TE_FLAG_PURE as libc::c_int,
                     context: 0 as *const libc::c_void as *mut libc::c_void,},
         te_variable{name: b"ln\x00" as *const u8 as *const libc::c_char,
                     address:
                         ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                 libc::c_double)
                                                            ->
                                                                libc::c_double>,
                                                 *const libc::c_void>(Some(log)),
                     type_0:
                         TE_FUNCTION1 as libc::c_int |
                             TE_FLAG_PURE as libc::c_int,
                     context: 0 as *const libc::c_void as *mut libc::c_void,},
         te_variable{name: b"log\x00" as *const u8 as *const libc::c_char,
                     address:
                         ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                 libc::c_double)
                                                            ->
                                                                libc::c_double>,
                                                 *const libc::c_void>(Some(log10)),
                     type_0:
                         TE_FUNCTION1 as libc::c_int |
                             TE_FLAG_PURE as libc::c_int,
                     context: 0 as *const libc::c_void as *mut libc::c_void,},
         te_variable{name: b"log10\x00" as *const u8 as *const libc::c_char,
                     address:
                         ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                 libc::c_double)
                                                            ->
                                                                libc::c_double>,
                                                 *const libc::c_void>(Some(log10)),
                     type_0:
                         TE_FUNCTION1 as libc::c_int |
                             TE_FLAG_PURE as libc::c_int,
                     context: 0 as *const libc::c_void as *mut libc::c_void,},
         te_variable{name: b"ncr\x00" as *const u8 as *const libc::c_char,
                     address:
                         ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                 libc::c_double,
                                                                             _:
                                                                                 libc::c_double)
                                                            ->
                                                                libc::c_double>,
                                                 *const libc::c_void>(Some(ncr)),
                     type_0:
                         TE_FUNCTION2 as libc::c_int |
                             TE_FLAG_PURE as libc::c_int,
                     context: 0 as *const libc::c_void as *mut libc::c_void,},
         te_variable{name: b"npr\x00" as *const u8 as *const libc::c_char,
                     address:
                         ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                 libc::c_double,
                                                                             _:
                                                                                 libc::c_double)
                                                            ->
                                                                libc::c_double>,
                                                 *const libc::c_void>(Some(npr)),
                     type_0:
                         TE_FUNCTION2 as libc::c_int |
                             TE_FLAG_PURE as libc::c_int,
                     context: 0 as *const libc::c_void as *mut libc::c_void,},
         te_variable{name: b"pi\x00" as *const u8 as *const libc::c_char,
                     address:
                         ::std::mem::transmute::<Option<unsafe extern "C" fn()
                                                            ->
                                                                libc::c_double>,
                                                 *const libc::c_void>(Some(pi)),
                     type_0:
                         TE_FUNCTION0 as libc::c_int |
                             TE_FLAG_PURE as libc::c_int,
                     context: 0 as *const libc::c_void as *mut libc::c_void,},
         te_variable{name: b"pow\x00" as *const u8 as *const libc::c_char,
                     address:
                         ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                 libc::c_double,
                                                                             _:
                                                                                 libc::c_double)
                                                            ->
                                                                libc::c_double>,
                                                 *const libc::c_void>(Some(pow)),
                     type_0:
                         TE_FUNCTION2 as libc::c_int |
                             TE_FLAG_PURE as libc::c_int,
                     context: 0 as *const libc::c_void as *mut libc::c_void,},
         te_variable{name: b"sin\x00" as *const u8 as *const libc::c_char,
                     address:
                         ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                 libc::c_double)
                                                            ->
                                                                libc::c_double>,
                                                 *const libc::c_void>(Some(sin)),
                     type_0:
                         TE_FUNCTION1 as libc::c_int |
                             TE_FLAG_PURE as libc::c_int,
                     context: 0 as *const libc::c_void as *mut libc::c_void,},
         te_variable{name: b"sinh\x00" as *const u8 as *const libc::c_char,
                     address:
                         ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                 libc::c_double)
                                                            ->
                                                                libc::c_double>,
                                                 *const libc::c_void>(Some(sinh)),
                     type_0:
                         TE_FUNCTION1 as libc::c_int |
                             TE_FLAG_PURE as libc::c_int,
                     context: 0 as *const libc::c_void as *mut libc::c_void,},
         te_variable{name: b"sqrt\x00" as *const u8 as *const libc::c_char,
                     address:
                         ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                 libc::c_double)
                                                            ->
                                                                libc::c_double>,
                                                 *const libc::c_void>(Some(sqrt)),
                     type_0:
                         TE_FUNCTION1 as libc::c_int |
                             TE_FLAG_PURE as libc::c_int,
                     context: 0 as *const libc::c_void as *mut libc::c_void,},
         te_variable{name: b"tan\x00" as *const u8 as *const libc::c_char,
                     address:
                         ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                 libc::c_double)
                                                            ->
                                                                libc::c_double>,
                                                 *const libc::c_void>(Some(tan)),
                     type_0:
                         TE_FUNCTION1 as libc::c_int |
                             TE_FLAG_PURE as libc::c_int,
                     context: 0 as *const libc::c_void as *mut libc::c_void,},
         te_variable{name: b"tanh\x00" as *const u8 as *const libc::c_char,
                     address:
                         ::std::mem::transmute::<Option<unsafe extern "C" fn(_:
                                                                                 libc::c_double)
                                                            ->
                                                                libc::c_double>,
                                                 *const libc::c_void>(Some(tanh)),
                     type_0:
                         TE_FUNCTION1 as libc::c_int |
                             TE_FLAG_PURE as libc::c_int,
                     context: 0 as *const libc::c_void as *mut libc::c_void,},
         te_variable{name: 0 as *const libc::c_char,
                     address: 0 as *const libc::c_void,
                     type_0: 0i32,
                     context: 0 as *const libc::c_void as *mut libc::c_void,}]
    };
unsafe extern "C" fn pi() -> libc::c_double { return 3.141592653589793f64; }
unsafe extern "C" fn npr(mut n: libc::c_double, mut r: libc::c_double)
 -> libc::c_double {
    return ncr(n, r) * fac(r);
}
unsafe extern "C" fn fac(mut a: libc::c_double) -> libc::c_double {
    if a < 0.0f64 {
        return ::std::f32::NAN as libc::c_double
    } else if a >
                  (2147483647i32 as
                       libc::c_uint).wrapping_mul(2u32).wrapping_add(1u32) as
                      libc::c_double {
        return ::std::f32::INFINITY as libc::c_double
    } else {
        let mut ua: libc::c_uint = a as libc::c_uint;
        let mut result: libc::c_ulong = 1i32 as libc::c_ulong;
        let mut i: libc::c_ulong = 0;
        i = 1i32 as libc::c_ulong;
        while i <= ua as libc::c_ulong {
            if i >
                   (9223372036854775807i64 as
                        libc::c_ulong).wrapping_mul(2u64).wrapping_add(1u64).wrapping_div(result)
               {
                return ::std::f32::INFINITY as libc::c_double
            } else { result = result.wrapping_mul(i); i = i.wrapping_add(1) }
        }
        return result as libc::c_double
    };
}
unsafe extern "C" fn ncr(mut n: libc::c_double, mut r: libc::c_double)
 -> libc::c_double {
    if n < 0.0f64 || r < 0.0f64 || n < r {
        return ::std::f32::NAN as libc::c_double
    } else if n >
                  (2147483647i32 as
                       libc::c_uint).wrapping_mul(2u32).wrapping_add(1u32) as
                      libc::c_double ||
                  r >
                      (2147483647i32 as
                           libc::c_uint).wrapping_mul(2u32).wrapping_add(1u32)
                          as libc::c_double {
        return ::std::f32::INFINITY as libc::c_double
    } else {
        let mut un: libc::c_ulong = n as libc::c_uint as libc::c_ulong;
        let mut ur: libc::c_ulong = r as libc::c_uint as libc::c_ulong;
        let mut i: libc::c_ulong = 0;
        let mut result: libc::c_ulong = 1i32 as libc::c_ulong;
        if ur > un.wrapping_div(2i32 as libc::c_ulong) {
            ur = un.wrapping_sub(ur)
        }
        i = 1i32 as libc::c_ulong;
        while i <= ur {
            if result >
                   (9223372036854775807i64 as
                        libc::c_ulong).wrapping_mul(2u64).wrapping_add(1u64).wrapping_div(un.wrapping_sub(ur).wrapping_add(i))
               {
                return ::std::f32::INFINITY as libc::c_double
            } else {
                result =
                    result.wrapping_mul(un.wrapping_sub(ur).wrapping_add(i));
                result = result.wrapping_div(i);
                i = i.wrapping_add(1)
            }
        }
        return result as libc::c_double
    };
}
unsafe extern "C" fn e() -> libc::c_double { return 2.718281828459045f64; }
unsafe extern "C" fn comma(mut a: libc::c_double, mut b: libc::c_double)
 -> libc::c_double {
    return b;
}
unsafe extern "C" fn optimize(mut n: *mut te_expr_0) -> () {
    /* Evaluates as much as possible. */
    if (*n).type_0 == TE_CONSTANT as libc::c_int {
        return
    } else if (*n).type_0 == TE_VARIABLE as libc::c_int {
        return
    } else {
        /* Only optimize out functions flagged as pure. */
        if (*n).type_0 & TE_FLAG_PURE as libc::c_int != 0i32 {
            let arity: libc::c_int =
                if 0 !=
                       (*n).type_0 &
                           (TE_FUNCTION0 as libc::c_int |
                                TE_CLOSURE0 as libc::c_int) {
                    (*n).type_0 & 7i32
                } else { 0i32 };
            let mut known: libc::c_int = 1i32;
            let mut i: libc::c_int = 0;
            i = 0i32;
            while i < arity {
                optimize((*n).parameters[i as usize] as *mut te_expr_0);
                if (*((*n).parameters[i as usize] as *mut te_expr_0)).type_0
                       != TE_CONSTANT as libc::c_int {
                    known = 0i32
                }
                i += 1
            }
            if 0 != known {
                let value: libc::c_double = te_eval(n);
                te_free_parameters(n);
                (*n).type_0 = TE_CONSTANT as libc::c_int;
                (*n).unnamed.value = value
            }
        }
        return;
    };
}
/* Evaluates the expression. */
#[no_mangle]
pub unsafe extern "C" fn te_eval(mut n: *const te_expr_0) -> libc::c_double {
    if n.is_null() {
        return ::std::f32::NAN as libc::c_double
    } else {
        match (*n).type_0 & 31i32 {
            1 => { return (*n).unnamed.value }
            0 => { return *(*n).unnamed.bound }
            8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 => {
                match if 0 !=
                             (*n).type_0 &
                                 (TE_FUNCTION0 as libc::c_int |
                                      TE_CLOSURE0 as libc::c_int) {
                          (*n).type_0 & 7i32
                      } else { 0i32 } {
                    0 => {
                        return ::std::mem::transmute::<*const libc::c_void,
                                                       Option<unsafe extern "C" fn()
                                                                  ->
                                                                      libc::c_double>>((*n).unnamed.function).expect("non-null function pointer")()
                    }
                    1 => {
                        return ::std::mem::transmute::<*const libc::c_void,
                                                       Option<unsafe extern "C" fn(_:
                                                                                       libc::c_double)
                                                                  ->
                                                                      libc::c_double>>((*n).unnamed.function).expect("non-null function pointer")(te_eval((*n).parameters[0usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0))
                    }
                    2 => {
                        return ::std::mem::transmute::<*const libc::c_void,
                                                       Option<unsafe extern "C" fn(_:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double)
                                                                  ->
                                                                      libc::c_double>>((*n).unnamed.function).expect("non-null function pointer")(te_eval((*n).parameters[0usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[1usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0))
                    }
                    3 => {
                        return ::std::mem::transmute::<*const libc::c_void,
                                                       Option<unsafe extern "C" fn(_:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double)
                                                                  ->
                                                                      libc::c_double>>((*n).unnamed.function).expect("non-null function pointer")(te_eval((*n).parameters[0usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[1usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[2usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0))
                    }
                    4 => {
                        return ::std::mem::transmute::<*const libc::c_void,
                                                       Option<unsafe extern "C" fn(_:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double)
                                                                  ->
                                                                      libc::c_double>>((*n).unnamed.function).expect("non-null function pointer")(te_eval((*n).parameters[0usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[1usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[2usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[3usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0))
                    }
                    5 => {
                        return ::std::mem::transmute::<*const libc::c_void,
                                                       Option<unsafe extern "C" fn(_:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double)
                                                                  ->
                                                                      libc::c_double>>((*n).unnamed.function).expect("non-null function pointer")(te_eval((*n).parameters[0usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[1usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[2usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[3usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[4usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0))
                    }
                    6 => {
                        return ::std::mem::transmute::<*const libc::c_void,
                                                       Option<unsafe extern "C" fn(_:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double)
                                                                  ->
                                                                      libc::c_double>>((*n).unnamed.function).expect("non-null function pointer")(te_eval((*n).parameters[0usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[1usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[2usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[3usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[4usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[5usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0))
                    }
                    7 => {
                        return ::std::mem::transmute::<*const libc::c_void,
                                                       Option<unsafe extern "C" fn(_:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double)
                                                                  ->
                                                                      libc::c_double>>((*n).unnamed.function).expect("non-null function pointer")(te_eval((*n).parameters[0usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[1usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[2usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[3usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[4usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[5usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[6usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0))
                    }
                    _ => { return ::std::f32::NAN as libc::c_double }
                }
            }
            16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 => {
                match if 0 !=
                             (*n).type_0 &
                                 (TE_FUNCTION0 as libc::c_int |
                                      TE_CLOSURE0 as libc::c_int) {
                          (*n).type_0 & 7i32
                      } else { 0i32 } {
                    0 => {
                        return ::std::mem::transmute::<*const libc::c_void,
                                                       Option<unsafe extern "C" fn(_:
                                                                                       *mut libc::c_void)
                                                                  ->
                                                                      libc::c_double>>((*n).unnamed.function).expect("non-null function pointer")((*n).parameters[0usize])
                    }
                    1 => {
                        return ::std::mem::transmute::<*const libc::c_void,
                                                       Option<unsafe extern "C" fn(_:
                                                                                       *mut libc::c_void,
                                                                                   _:
                                                                                       libc::c_double)
                                                                  ->
                                                                      libc::c_double>>((*n).unnamed.function).expect("non-null function pointer")((*n).parameters[1usize],
                                                                                                                                                  te_eval((*n).parameters[0usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0))
                    }
                    2 => {
                        return ::std::mem::transmute::<*const libc::c_void,
                                                       Option<unsafe extern "C" fn(_:
                                                                                       *mut libc::c_void,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double)
                                                                  ->
                                                                      libc::c_double>>((*n).unnamed.function).expect("non-null function pointer")((*n).parameters[2usize],
                                                                                                                                                  te_eval((*n).parameters[0usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[1usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0))
                    }
                    3 => {
                        return ::std::mem::transmute::<*const libc::c_void,
                                                       Option<unsafe extern "C" fn(_:
                                                                                       *mut libc::c_void,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double)
                                                                  ->
                                                                      libc::c_double>>((*n).unnamed.function).expect("non-null function pointer")((*n).parameters[3usize],
                                                                                                                                                  te_eval((*n).parameters[0usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[1usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[2usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0))
                    }
                    4 => {
                        return ::std::mem::transmute::<*const libc::c_void,
                                                       Option<unsafe extern "C" fn(_:
                                                                                       *mut libc::c_void,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double)
                                                                  ->
                                                                      libc::c_double>>((*n).unnamed.function).expect("non-null function pointer")((*n).parameters[4usize],
                                                                                                                                                  te_eval((*n).parameters[0usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[1usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[2usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[3usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0))
                    }
                    5 => {
                        return ::std::mem::transmute::<*const libc::c_void,
                                                       Option<unsafe extern "C" fn(_:
                                                                                       *mut libc::c_void,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double)
                                                                  ->
                                                                      libc::c_double>>((*n).unnamed.function).expect("non-null function pointer")((*n).parameters[5usize],
                                                                                                                                                  te_eval((*n).parameters[0usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[1usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[2usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[3usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[4usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0))
                    }
                    6 => {
                        return ::std::mem::transmute::<*const libc::c_void,
                                                       Option<unsafe extern "C" fn(_:
                                                                                       *mut libc::c_void,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double)
                                                                  ->
                                                                      libc::c_double>>((*n).unnamed.function).expect("non-null function pointer")((*n).parameters[6usize],
                                                                                                                                                  te_eval((*n).parameters[0usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[1usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[2usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[3usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[4usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[5usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0))
                    }
                    7 => {
                        return ::std::mem::transmute::<*const libc::c_void,
                                                       Option<unsafe extern "C" fn(_:
                                                                                       *mut libc::c_void,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double,
                                                                                   _:
                                                                                       libc::c_double)
                                                                  ->
                                                                      libc::c_double>>((*n).unnamed.function).expect("non-null function pointer")((*n).parameters[7usize],
                                                                                                                                                  te_eval((*n).parameters[0usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[1usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[2usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[3usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[4usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[5usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0),
                                                                                                                                                  te_eval((*n).parameters[6usize]
                                                                                                                                                              as
                                                                                                                                                              *const te_expr_0))
                    }
                    _ => { return ::std::f32::NAN as libc::c_double }
                }
            }
            _ => { return ::std::f32::NAN as libc::c_double }
        }
    };
}
#[no_mangle]
pub unsafe extern "C" fn te_free_parameters(mut n: *mut te_expr_0) -> () {
    let mut current_block: u64;
    if n.is_null() {
        return
    } else {
        match (*n).type_0 & 31i32 {
            15 | 23 => {
                /* Falls through. */
                te_free((*n).parameters[6usize] as *mut te_expr_0);
                current_block = 6269221470219320715;
            }
            14 | 22 => { current_block = 6269221470219320715; }
            13 | 21 => { current_block = 13041560561293621275; }
            12 | 20 => { current_block = 18052238657935791076; }
            11 | 19 => { current_block = 1945529299736923098; }
            10 | 18 => { current_block = 11977702171509306225; }
            9 | 17 => { current_block = 7216536457578139154; }
            _ => { current_block = 12675440807659640239; }
        }
        match current_block {
            6269221470219320715 => {
                /* Falls through. */
                te_free((*n).parameters[5usize] as *mut te_expr_0);
                current_block = 13041560561293621275;
            }
            _ => { }
        }
        match current_block {
            13041560561293621275 => {
                /* Falls through. */
                te_free((*n).parameters[4usize] as *mut te_expr_0);
                current_block = 18052238657935791076;
            }
            _ => { }
        }
        match current_block {
            18052238657935791076 => {
                /* Falls through. */
                te_free((*n).parameters[3usize] as *mut te_expr_0);
                current_block = 1945529299736923098;
            }
            _ => { }
        }
        match current_block {
            1945529299736923098 => {
                /* Falls through. */
                te_free((*n).parameters[2usize] as *mut te_expr_0);
                current_block = 11977702171509306225;
            }
            _ => { }
        }
        match current_block {
            11977702171509306225 => {
                /* Falls through. */
                te_free((*n).parameters[1usize] as *mut te_expr_0);
                current_block = 7216536457578139154;
            }
            _ => { }
        }
        match current_block {
            7216536457578139154 => {
                te_free((*n).parameters[0usize] as *mut te_expr_0);
            }
            _ => { }
        }
        return;
    };
}
/* Frees the expression. */
/* This is safe to call on NULL pointers. */
#[no_mangle]
pub unsafe extern "C" fn te_free(mut n: *mut te_expr_0) -> () {
    if n.is_null() {
        return
    } else { te_free_parameters(n); free(n as *mut libc::c_void); return; };
}
/* Prints debugging information on the syntax tree. */
#[no_mangle]
pub unsafe extern "C" fn te_print(mut n: *const te_expr_0) -> () {
    pn(n, 0i32);
}
unsafe extern "C" fn pn(mut n: *const te_expr_0, mut depth: libc::c_int)
 -> () {
    let mut i: libc::c_int = 0;
    let mut arity: libc::c_int = 0;
    printf(b"%*s\x00" as *const u8 as *const libc::c_char, depth,
           b"\x00" as *const u8 as *const libc::c_char);
    match (*n).type_0 & 31i32 {
        1 => {
            printf(b"%f\n\x00" as *const u8 as *const libc::c_char,
                   (*n).unnamed.value);
        }
        0 => {
            printf(b"bound %p\n\x00" as *const u8 as *const libc::c_char,
                   (*n).unnamed.bound);
        }
        8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22
        | 23 => {
            arity =
                if 0 !=
                       (*n).type_0 &
                           (TE_FUNCTION0 as libc::c_int |
                                TE_CLOSURE0 as libc::c_int) {
                    (*n).type_0 & 7i32
                } else { 0i32 };
            printf(b"f%d\x00" as *const u8 as *const libc::c_char, arity);
            i = 0i32;
            while i < arity {
                printf(b" %p\x00" as *const u8 as *const libc::c_char,
                       (*n).parameters[i as usize]);
                i += 1
            }
            printf(b"\n\x00" as *const u8 as *const libc::c_char);
            i = 0i32;
            while i < arity {
                pn((*n).parameters[i as usize] as *const te_expr_0,
                   depth + 1i32);
                i += 1
            }
        }
        _ => { }
    };
}
