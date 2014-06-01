import cog
from string import Template

def tp(template, d):
    return Template(template).substitute(d)

def header():
    cog.outl('// *** Generated section: do not edit until the end marker')

def definition(args):
    return ', '.join(' '.join(i) for i in args)

# We take the *last* element of the tuple/list here so that argument
# names can be rewritten if they are modified within the body of a
# function.  It's a hack for sure.
def call(args):
    return ', '.join([i[-1] for i in args])

## TODO: Once these are working, replace the XPtr stuff with a
## reference and see if that actually works.  If it does it will
## be nice.

## There are a couple of different cases that we deal with here:
##
## Call a const method of a class.
##   - Can take a const reference as the type, which will always do
##     pointer validity checking
##   - Always returns a value, so we need to know type
##
## Call a function that takes a const reference
##   - basically the same as the above.
##
## Call a nonconst method of a class.
##   - Need to take a pointer
##   - Never returns a value

def export_const_method(class_name, method, return_type, args=[]):
    template = """// [[Rcpp::export]]
${return_type} ${class_name}__${method}(${definition}) {
  return tr.${method}(${call});
}
"""
    args = [('const forest::%s&' % class_name, 'tr')] + args
    d = {'class_name': class_name, 'method': method,
         'return_type': return_type,
         'definition': definition(args),
         'call': call(args[1:])}
    cog.out(tp(template, d))

def export_const_function(class_name, function, return_type, args=[]):
    template = """// [[Rcpp::export]]
${return_type} ${class_name}__${function}(${definition}) {
  return forest::${function}(${call});
}
"""
    args = [('const forest::%s&' % class_name, 'tr')] + args
    d = {'class_name': class_name, 'function': function,
         'return_type': return_type,
         'definition': definition(args),
         'call': call(args)}
    cog.out(tp(template, d))

def export_nonconst(class_name, function, args=[]):
    template = """// [[Rcpp::export]]
void ${class_name}__${function}(${definition}) {
  forest::util::check_ptr_valid(ptr);
  forest::${function}(${call});
}
"""
    args = [('Rcpp::XPtr<forest::%s>' % class_name, 'ptr')] + args
    d = {'class_name': class_name, 'function': function,
         'definition': definition(args),
         'call': '*' + call(args)}
    cog.out(tp(template, d))

## There is a general form in here somewhere that the other functions
## could use.  A similar version can be written for const_ref
def export_ptr(class_name, function, return_type='void', args=[]):
    template = """// [[Rcpp::export]]
${return_type} ${class_name}__${function}(${definition}) {
  forest::util::check_ptr_valid(ptr);
  ${return_statement}forest::${function}(${call});
}
"""
    args = [('Rcpp::XPtr<forest::%s>' % class_name, 'ptr')] + args
    d = {'class_name': class_name, 'function': function,
         'definition': definition(args),
         'return_type': return_type,
         'return_statement': '' if return_type == 'void' else 'return ',
         'call': '*' + call(args)}
    cog.out(tp(template, d))

# Untested but looks about right.
# def export_cr(class_name, function, return_type='void', args=[]):
#     template = """// [[Rcpp::export]]
# ${return_type} ${class_name}__${function}(${definition}) {
#   ${return_statement}forest::${function}(${call});
# }
# """
#     args = [('const forest::%s&' % class_name, 'tr')] + args
#     d = {'class_name': class_name, 'function': function,
#          'definition': definition(args),
#          'return_type': return_type,
#          'return_statement': '' if return_type == 'void' else 'return ',
#          'call': '*' + call(args)}
#     cog.out(tp(template, d))

# This is really annoying - we need to both check and sanitise the
# index.  Another way of doing this would be to declare a type
# "Index", which we could range check automatically on the way in.
# Still will need an extra line though.  That also means that we
# change the *name* of the index argument and this ends up as a
# complete mess.
def export_child(class_name, value_type, get):
    template = """// [[Rcpp::export]]
${return_type} ${class_name}__${function}(${definition}) {
  forest::util::check_ptr_valid(ptr);
  size_t i = forest::util::safe_index_from_r(idx, ptr->arity());
  ${return_statement}forest::${function}(${call});
}
"""
    return_type = 'forest::forest_' + value_type if get else 'void'
    function = 'child_' + value_type
    args = [('Rcpp::XPtr<forest::%s>' % class_name, 'ptr'),
            ('int', 'idx', 'i')]
    if not get:
        args += [(value_type, 'value')]
        function = 'set_' + function
    d = {'class_name': class_name, 'function': function,
         'definition': definition(args),
         'return_type': return_type,
         'return_statement': '' if return_type == 'void' else 'return ',
         'call': '*' + call(args)}
    cog.out(tp(template, d))
