# LEGO type:standard slot:8 autostart

# CSE-style interpreter for Source §3 (JavaScript sublanguage),
# written in MicroPython (sublanguage of Python 3)

# author: Martin Henz, henz@comp.nus.edu.sg

# not available in MicroPython:
# match ... case statement: use dictionary of lambdas

# comment this for testing with Python 3
# uncomment this for testing with MicroPython

import json
import math
import random
import time

######################
# useful helpers
######################


def reduce(f, iterable, init):
    # Reimplement reduce because functools not in Micropython
    res = init
    for x in iterable:
        res = f(res, x)
    return res


def scheme_map(f, iterable):
    '''
    Maps like Scheme's `map`, not like Python's `map`
    '''
    def g(acc, x):
        acc.append(f(x))
        return acc
    return reduce(g, iterable, [])

######################
# JavaScript values
######################


# We represent JS null by Python None
Null = None

# We need a special value in Python to
# represent JS undefined. (null already taken)
# We (ab)use Python's Ellipsis value for JS undefined.
Undefined = ...


def is_closure(x):
    # more pythonic would be isinstance(v, Dict),
    # but package typing not available in Micropython
    return type(x) is dict and 'tag' in x and x['tag'] == 'closure'


def is_builtin(x):
    return type(x) is dict and 'tag' in x and x['tag'] == 'builtin'


def is_string(x):
    return type(x) is str


def value_to_string(x):
    # Printing not speed-critical, so nested conditionals are fine
    if x is Undefined:
        return 'undefined'
    elif x is True:
        return 'true'
    elif x is False:
        return 'false'
    elif x is Null:
        return 'null'
    elif is_string(x):
        # surround characters with double quotes;
        # escape double quote characters within string
        return '"' + reduce(lambda s, t: s + ('\\"' if t == '"' else t), x, '') + '"'
    elif is_closure(x):
        return '<closure>'
    elif is_builtin(x):
        return '<builtin>'
    elif type(x) is float:
        return str(int(x)) if x == math.floor(x) else str(x)
    else:
        return str(x)

########################
# operators and builtins
########################


binop_microcode = {
    '+': lambda x, y: x + y if (type(x) is float and type(y) is float) or
    (type(x) is str and type(y) is str)
    else raise_exception("+ expects two numbers or two strings"),
    '*': lambda x, y: x * y,
    '-': lambda x, y: x - y,
    '/': lambda x, y: x / y,
    '%': lambda x, y: x % y,
    '<': lambda x, y: x < y,
    '<=': lambda x, y: x <= y,
    '>=': lambda x, y: x >= y,
    '>': lambda x, y: x > y,
    '===': lambda x, y: x is y,
    '!==': lambda x, y: not (x is y)
}


def apply_binop(op, v2, v1):
    # `v2` is popped before `v1`
    return binop_microcode[op](v1, v2)


unop_microcode = {
    '-unary': lambda x: - x,
    '!': lambda x: not x if x == bool(x)
    else raise_exception('! expects boolean')
}


def apply_unop(op, v):
    return unop_microcode[op](v)


def builtin_microcode_display(args):
    s = str(args[0])
    if len(args) == 2:
        if type(args[1]) is str:
            s = args[1] + ' ' + s
        else:
            raise Exception("display expects string as second argument")
    elif len(args) != 1:
        raise Exception("display expects one or two arguments")
    print(s)
    return args[0]


def builtin_microcode_error(args):
    s = str(args[0])
    if len(args) == 2:
        if type(args[1]) is str:
            s = args[1] + ' ' + s
        else:
            raise Exception("error expects string as second argument")
    elif len(args) != 1:
        raise Exception("error expects one or two arguments")
    raise Exception(s)


def builtin_microcode_clz32(args):
    if args[0] < 0:
        return 0
    return float(32 - int(args[0]).bit_length())


def builtin_microcode_set_head(args):
    args[0][0] = args[1]
    return Undefined


def builtin_microcode_set_tail(args):
    args[0][1] = args[1]
    return Undefined


def builtin_microcode_list(args):
    result = Null
    while args:
        result = [args.pop(), result]
    return result


def is_list(val):
    return val == Null or \
        (type(val) is list and len(val) == 2 and
            is_list(val[1]))


def builtin_microcode_is_list(args):
    return is_list(args[0])


builtin_microcode = {
    'display': lambda args: builtin_microcode_display(args),
    'get_time': lambda _: float(int(time.time() * 1000)),
    'stringify': lambda args: str(args[0]),
    'error': builtin_microcode_error,
    'prompt': lambda args: input(args[0]),
    'is_number': lambda args: type(args[0]) is float,
    'is_string': lambda args: type(args[0]) is str,
    'is_function': lambda args: type(args[0]) is dict and
    (args[0]['tag'] == 'builtin' or
     args[0]['tag'] == 'closure'),
    'is_boolean': lambda args: type(args[0]) is bool,
    'is_undefined': lambda args: args[0] is Undefined,
    'parse_int': lambda args: float(int(args[0], base=int(args[1]))),
    'char_at': lambda args: str(args[0][int(args[1])]),
    'arity': lambda args: float(args[0]['arity']),
    'math_abs': lambda args: float(abs(args[0])),
    'math_acos': lambda args: float(math.acos(args[0])),
    'math_acosh': lambda args: float(math.acosh(args[0])),
    'math_asin': lambda args: float(math.asin(args[0])),
    'math_asinh': lambda args: float(math.asinh(args[0])),
    'math_atan': lambda args: float(math.atan(args[0])),
    'math_atanh': lambda args: float(math.atanh(args[0])),
    'math_atan2': lambda args: float(math.atan2(args[0], args[1])),
    'math_ceil': lambda args: float(math.ceil(args[0])),
    'math_cbrt': lambda args: float(args[0]**(1/3)),
    'math_expm1': lambda args: float(math.expm1(args[0])),
    'math_clz32': lambda args: float(builtin_microcode_clz32(args[0])),
    'math_cos': lambda args: float(math.cos(args[0])),
    'math_cosh': lambda args: float(math.cosh(args[0])),
    'math_exp': lambda args: float(math.exp(args[0])),
    'math_floor': lambda args: float(math.floor(args[0])),
    'math_fround': lambda args: float(args[0]),          # FIXME
    'math_hypot': lambda args: float(args[0]),           # FIXME
    'math_imul': lambda args: float(args[0] * args[1]),  # FIXME
    'math_log': lambda args: float(math.log(args[0])),
    'math_log1p': lambda args: float(math.log1p(args[0])),
    'math_log2': lambda args: float(math.log2(args[0])),
    'math_log10': lambda args: float(math.log10(args[0])),
    'math_max': lambda args: float(max(args)),
    'math_min': lambda args: float(min(args)),
    'math_pow': lambda args: math.pow(args[0], args[1]),
    'math_random': lambda args: float(random.random()),
    'math_round': lambda args: float(round(args[0])),
    'math_sign': lambda args: float(0 if args[0] == 0 else math.copysign(1, args[0])),
    'math_sin': lambda args: float(math.sin(args[0])),
    'math_sinh': lambda args: float(math.sinh(args[0])),
    'math_sqrt': lambda args: float(math.sqrt(args[0])),
    'math_tanh': lambda args: float(math.tanh(args[0])),
    'math_trunc': lambda args: float(math.trunc(args[0])),
    'pair': lambda args: [args[0], args[1]],
    'is_pair': lambda args: type(args[0]) == list and len(args[0] == 2),
    'head': lambda args: args[0][0],
    'tail': lambda args: args[0][1],
    'is_null': lambda args: args[0] is None,
    'set_head': builtin_microcode_set_head,
    'set_tail': builtin_microcode_set_tail,
    'array_length': lambda args: len(args[0]),
    'is_array': lambda args: type(args[0]) is list,
    'list': builtin_microcode_list,
    'is_list': builtin_microcode_is_list,
    'display_list': builtin_microcode_display,  # FIXME
    # stream_tail: moved to prelude
    # stream: moved to prelude
}


def apply_builtin(builtin_symbol, arity, args):
    return builtin_microcode[builtin_symbol](args)

####################
# environments
####################

# frames are dictionaries mapping symbols (strings) to values


# the global frame has all builtins
builtin_names = tuple(builtin_microcode.keys())

# builtin arities other than 1
builtin_arities = {
    'list': 0,
    'stream': 0,
    'get_time': 0,
    'parse_int': 2,
    'char_at': 2,
    'math_atan2': 2,
    'math_hypot': 2,  # according to JS: Math.hypot.length
    'math_min': 2,  # according to JS: Math.min.length
    'math_max': 2,  # according to JS: Math.min.length
    'math_pow': 2,
    'math_random': 0,
    'pair': 2,
    'set_head': 2,
    'set_tail': 2,
    'list': 0,  # according to Source: arity(list)
    'stream': 0,  # according to Source: arity(list)
}

global_frame = {}
for b in builtin_names:
    global_frame[b] = {'tag': 'builtin',
                       'sym': b,
                       'arity': builtin_arities[b]
                       if b in builtin_arities
                       else 1
                       }
global_frame['undefined'] = Undefined
global_frame['math_E'] = math.e
global_frame['math_LN10'] = math.log(10)
global_frame['math_LN2'] = math.log(2)
global_frame['math_LOG10E'] = math.log10(math.e)
global_frame['math_LOG2E'] = math.log2(math.e)
global_frame['math_PI'] = math.pi
global_frame['math_SQRT1_2'] = math.sqrt(0.5)
global_frame['math_SQRT2'] = math.sqrt(2)

'''
# include the following lines for working on robot
global_frame['PrimeHub'] = PrimeHub
global_frame['LightMatrix'] = LightMatrix
global_frame['Button'] = Button
global_frame['StatusLight'] = StatusLight
global_frame['ForceSensor'] = ForceSensor
global_frame['MotionSensor'] = MotionSensor
global_frame['Speaker'] = Speaker
global_frame['ColorSensor'] = ColorSensor
global_frame['App'] = App
global_frame['DistanceSensor'] = DistanceSensor
global_frame['Motor'] = Motor
global_frame['MotorPair'] = MotorPair
'''

# environments are None or
# pairs whose head is a frame
# and whose tail is an environment
empty_environment = None
global_environment = (global_frame, empty_environment)


def lookup(x, e):
    res = lookup_(x, e, 0)
    return res[0]


def lookup_(x, e, i):
    if e == None:
        raise Exception('unbound name: ' + x)
    if x in e[0]:
        return (e[0][x], i)
    else:
        return lookup_(x, e[1], i + 1)


def extend(xs, vs, e):
    vi = iter(vs)
    result = {}
    for x in xs:
        result[x] = next(vi)
    return (result, e)


def replace(x, v, e):
    if x in e[0]:
        e[0][x] = v
    else:
        replace(x, v, e[1])

####################
# handling sequences
####################


def value_producing(cmd):
    tag = cmd['tag']
    return tag != 'let' and \
        tag != 'const' and \
        tag != 'fun' and \
        (tag != 'blk' or value_producing(cmd['body'])) and \
        (tag != 'seq' or any(scheme_map(value_producing, cmd['stmts'])))


def handle_sequence(seq):
    value_produced = False
    result = []
    for cmd in seq:
        if value_producing(cmd):
            if value_produced:
                result.append({'tag': 'pop_i'})
            else:
                value_produced = True
        result.append(cmd)
    result.reverse()
    return result

####################
# handling blocks
####################


def scan(t):
    '''
    Scans out the declarations from (possibly nested)
    sequences of statements, ignoring blocks
    '''
    def f(acc, x):
        acc.extend(scan(x))
        return acc
    tag = t['tag']
    if tag == 'seq':
        return reduce(f,
                      t['stmts'],
                      [])
    elif tag == 'let' or tag == 'const' or tag == 'fun':
        return [t['sym']]
    else:
        return []

###############################
# machine overview
###############################

# CSE machine has three registers:
# C: control
# S: stash
# E: environment

###############################
# control C
###############################

# control C is list of commands that still need
# to be executed by the machine

# list follows strict stack discipline:
# pop, extend, append at end of list

# commands are nodes of syntax tree
# or instructions

# instructions are dictionaries whose
# 'tag' ends with '_i'

# control C starts out as singleton list containing
# the given program, wrapped in a block so that
# names declared at program level get allocated
# in a program frame

###############################
# stash S
###############################

# stash S is list of values that stores
# intermediate results

# list follows strict stack discipline:
# pop, extend, append at end of list

# stash S starts out as empty stack
S = []

###############################
# environment E
###############################

# environment E starts out as
# the global environment

E = global_environment

###############################
# the machine
###############################

# the machine dispaches for each command tag
# to the microcode that belong to the tag

# microcode is a function that takes
# command as argument and returns None


def cse_microcode_nam(cmd):
    v = lookup(cmd['sym'], E)
    # more pythonic would be isinstance(v, Dict),
    # but package typing not available in Micropython
    if type(v) is dict and 'tag' in v and v['tag'] == 'unassigned':
        raise Exception('unbound local: ' + cmd['sym'])
    else:
        S.append(v)


def cse_microcode_app_i(cmd):
    arity = cmd['arity']
    args = []
    for _ in range(arity):
        args.append(S.pop())
    sf = S.pop()
    if sf['tag'] == 'builtin':
        S.append(apply_builtin(sf['sym'], arity, args))
    elif sf['tag'] == 'closure':
        global E
        if not C or C[0]['tag'] == 'reset':  # tail call
            C.append(sf['body'])
        elif C[0]['tag'] == 'env_i':  # E no longer needed
            C.extend([{'tag': 'mark'},
                      sf['body']])
        else:
            C.extend([{'tag': 'env_i', 'env': E},
                      {'tag': 'mark'},
                      sf['body']])
        last_arg = args[-1]
        # Source allows last arg to be spread: ...x
        if type(last_arg) is dict and last_arg['tag'] == 'spread':
            spread_list = lookup(last_arg['sym'], E)
            if not (type(spread_list) is list):
                raise Exception('Source only spreads arrays')
            args.pop()
            args.extend(spread_list)
        prms = sf['prms']
        last_prm = prms[-1]
        if type(last_prm) is dict and last_prm['tag'] == 'rest':
            rest_sym = prms.pop()['sym']
            number_of_regular_prms = len(prms)
            prms.append(rest_sym)
            number_of_args = len(args)
            if number_of_args < number_of_regular_prms:
                raise Exception('too few arguments')
            new_args = args[0:number_of_regular_prms]
            rest_args = args[number_of_regular_prms:number_of_args]
            new_args.append(rest_args)
            args = new_args
        if len(args) > len(prms):
            raise Exception('too many arguments')
        if len(args) < len(prms):
            raise Exception('too few arguments')
        E = extend(sf['prms'], args, E)


def cse_microcode_blk(cmd):
    global E
    locals = scan(cmd['body'])
    unassigneds = scheme_map(lambda _: {'tag': 'unassigned'}, locals)
    C.extend([{'tag': 'env_i', 'env': E}, cmd['body']])
    E = extend(locals, unassigneds, E)


def cse_microcode_arr_lit_i(cmd):
    global S
    a = cmd['arity']
    array = S[- a - 1: len(S)]
    S = S[0: - a]
    S.append(array)


def cse_microcode_arr_acc_i(cmd):
    global S
    ind = S.pop()
    arr = S.pop()
    if type(ind) is dict and 'tag' in ind and ind['tag'] == 'prop':
        S.append(getattr(arr, ind['sym']))
    else:
        S.append(arr[ind])


def cse_microcode_arr_assmt_i(cmd):
    global S
    val = S.pop()
    ind = S.pop()
    arr = S.pop()
    arr[ind] = val
    S.append(val)


def cse_microcode_env_i(cmd):
    global E
    E = cmd['env']


def rev(xs):
    xs.reverse()
    return xs


def raise_exception(s): raise Exception(s)


cse_microcode = {
    ##########################
    # expressions of Source §3
    ##########################
    'lit':
    lambda cmd: S.append(float(cmd['val']) if type(
        cmd['val']) is int else cmd['val']),
    'nam':
    cse_microcode_nam,
    'unop':
    lambda cmd: C.extend([{'tag': 'unop_i', 'sym': cmd['sym']},
                          cmd['frst']]),
    'binop':
    lambda cmd: C.extend([{'tag': 'binop_i', 'sym': cmd['sym']},
                          cmd['scnd'],
                          cmd['frst']]),
    'log':
    lambda cmd: C.append({'tag': 'cond_expr',
                          'pred': cmd['frst'],
                          'cons': {'tag': 'lit', 'val': True},
                          'alt': cmd['scnd']}
                         if cmd['sym'] == '&&'
                         else
                         {'tag': 'cond_expr',
                          'pred': cmd['frst'],
                          'cons': cmd['scnd'],
                          'alt': {'tag': 'lit', 'val': False}}),
    'cond_expr':
    lambda cmd: C.extend([{'tag': 'branch_i',
                           'cons': cmd['cons'],
                           'alt': cmd['alt']},
                          cmd['pred']
                          ]),
    'app':
    lambda cmd: C.extend(
        [{'tag': 'app_i', 'arity': len(cmd['args'])}] + cmd['args'] + [cmd['fun']]),
    'assmt':
    lambda cmd: C.extend([{'tag': 'assmt_i', 'sym': cmd['sym']},
                          cmd['expr']]),
    'lam':
    lambda cmd: S.append({'tag': 'closure',
                          'prms': cmd['prms'],
                          'body': cmd['body'],
                          'env': E}),
    'spread':
    lambda cmd: S.append(cmd),
    'arr_lit':
    lambda cmd: C.extend(
        [{'tag': 'arr_lit_i', 'arity': len(cmd['elems'])}] + rev(cmd['elems'])),
    'arr_acc':
    lambda cmd: C.extend([{'tag': 'arr_acc_i'},
                          cmd['ind'],
                          cmd['arr']]),
    'arr_assmt':
    lambda cmd: C.extend([{'tag': 'arr_assmt_i'},
                          cmd['expr'],
                          cmd['ind'],
                          cmd['arr']]),
    #########################
    # statements of Source §3
    #########################
    'import':
    lambda _: raise_exception('import not available in MicroSource'),
    'seq':
    lambda cmd: C.extend(handle_sequence(cmd['stmts'])),
    'cond_stmt':
    # conditional statements are value-producing; thus
    # insert push_undefined_if_needed_i instruction
    # (as optimization, we could avoid this for non-top-level)
    lambda cmd: C.extend([{'tag': 'push_undefined_if_needed_i'},
                          {'tag': 'branch_i',
                           'cons': cmd['cons'],
                           'alt': cmd['alt']},
                          cmd['pred']
                          ]),
    'blk':
    cse_microcode_blk,
    # let and const are not value-producing,
    # but assmt pushes value. thus need to pop
    'let':
    lambda cmd: C.extend([{'tag': 'pop_i'},
                          {'tag': 'assmt',
                           'sym': cmd['sym'],
                           'expr': cmd['expr']}]),
    'const':
    lambda cmd: C.extend([{'tag': 'pop_i'},
                          {'tag': 'assmt',
                           'sym': cmd['sym'],
                           'expr': cmd['expr']}]),
    'ret':
    lambda cmd: C.extend([{'tag': 'reset_i'},
                          cmd['expr']]),
    'fun':
    lambda cmd: C.append({'tag': 'const',
                          'sym': cmd['sym'],
                          'expr': {'tag': 'lam',
                                   'prms': cmd['prms'],
                                   'body': cmd['body']}
                          }),
    'while':
    lambda cmd: C.extend([{'tag': 'while_i',
                           'pred': cmd['pred'],
                           'body': cmd['body']},
                          cmd['pred'],
                          {'tag': 'lit', 'val': Undefined}]),
    'for':  # follow https://docs.sourceacademy.org/source_3.pdf
    lambda cmd: C.append(
        {'tag': 'blk',
         'body':
         {'tag': 'seq',
          'stmts':
          [cmd['init'],
           {'tag': 'while',
            'pred': cmd['pred'],
            'body':
            {'tag': 'blk',
             'body':
             {'tag': 'seq',
              'stmts':
              [{'tag': 'const',
                'sym': '_copy_of_' +
                cmd['init']['sym'],
                'expr': {'tag': 'nam',
                         'sym': cmd['init']['sym']}
                },
               {'tag': 'blk',
                'body':
                {'tag': 'seq',
                 'stmts':
                 [{'tag': 'const',
                   'sym': cmd['init']['sym'],
                   'expr': {'tag': 'nam',
                            'sym': '_copy_of_' +
                            cmd['init']['sym']}},
                  cmd['body']]}},
               cmd['upd']
               ]}}}]}})
    if cmd['init']['tag'] == 'let'
    else
    C.append(
        {'tag': 'seq',
         'stmts':
            [cmd['init'],
             {'tag': 'while',
              'pred': cmd['pred'],
              'body':
                 {'tag': 'seq',
                  'stmts':
                     [cmd['body'],
                      cmd['upd']
                      ]}}]}),
    'prop':
    lambda cmd: S.append(cmd),
    #############################
    # instructions of CSE machine
    #############################
    'reset_i':
    lambda cmd: None
    if C.pop()['tag'] == 'mark'  # mark found: stop loop
    else C.append(cmd),  # otherwise continue loop
    'while_i':
    lambda cmd: C.extend([cmd,
                          cmd['pred'],
                          {'tag': 'push_undefined_if_needed_i'},
                          cmd['body'],
                          {'tag': 'pop_i'}])  # pop previous body value
    if S.pop()
    else None,
    'assmt_i':  # peek top of stash without popping:
    # assignments are value-producing
    lambda cmd: replace(cmd['sym'], S[-1], E),
    'unop_i':
    lambda cmd: S.append(apply_unop(cmd['sym'], S.pop())),
    'binop_i':
    lambda cmd: S.append(apply_binop(cmd['sym'], S.pop(), S.pop())),
    'pop_i':
    lambda _: S.pop(),
    'app_i':
    cse_microcode_app_i,
    'branch_i':
    lambda cmd: C.append(cmd['cons'] if S.pop() else cmd['alt']),
    'env_i':
    cse_microcode_env_i,
    'push_undefined_if_needed_i':
    lambda _: None if S else S.append(Undefined),
    'arr_lit_i':
    cse_microcode_arr_lit_i,
    'arr_acc_i':
    cse_microcode_arr_acc_i,
    'arr_assmt_i':
    cse_microcode_arr_assmt_i
}

json_string = '{"tag":"for","init":{"tag":"let","sym":"i","expr":{"tag":"lit","val":0}},"pred":{"tag":"binop","sym":"<","frst":{"tag":"nam","sym":"i"},"scnd":{"tag":"lit","val":10}},"upd":{"tag":"assmt","sym":"i","expr":{"tag":"binop","sym":"+","frst":{"tag":"nam","sym":"i"},"scnd":{"tag":"lit","val":1}}},"body":{"tag":"seq","stmts":[{"tag":"app","fun":{"tag":"nam","sym":"spike_showImage"},"args":[{"tag":"lit","val":"00099:00990:09900:99000:09900"}]},{"tag":"app","fun":{"tag":"nam","sym":"spike_sleep"},"args":[{"tag":"lit","val":0.2}]},{"tag":"app","fun":{"tag":"nam","sym":"spike_showImage"},"args":[{"tag":"lit","val":"00990:09900:99000:09900:00990"}]},{"tag":"app","fun":{"tag":"nam","sym":"spike_sleep"},"args":[{"tag":"lit","val":0.2}]},{"tag":"app","fun":{"tag":"nam","sym":"spike_showImage"},"args":[{"tag":"lit","val":"09900:99000:09900:00990:00099"}]},{"tag":"app","fun":{"tag":"nam","sym":"spike_sleep"},"args":[{"tag":"lit","val":0.2}]}]}}'

# evaluation of toplevel results
# in the value undefined if the
# stash is empty
C = [{'tag': 'push_undefined_if_needed_i'},
     {'tag': 'blk',
      'body': json.loads(json_string)}]
print("input: " + str(C))

# machine loops until control is empty
while True:
    if not C:
        break
    cmd = C.pop()
    cse_microcode[cmd['tag']](cmd)

if len(S) > 1 or len(S) < 1:
    raise Exception('internal error: stash must be singleton but is: ', S)
print("output: " + value_to_string(S[0]))
