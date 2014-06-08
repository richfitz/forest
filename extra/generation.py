import cog
from string import Template

def tp(template, d):
    return Template(template).substitute(d)

def header():
    cog.outl('// *** Generated section: do not edit until the end marker')

def definition(args):
    return ', '.join(' '.join(i[:2]) for i in args)

# We take the *last* element of the tuple/list here so that argument
# names can be rewritten if they are modified within the body of a
# function.  So the tuples/lists here really should be 2 or 3 elements
# long.
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
##
## Call a function that changes the tree
##   - Need to take a pointer
##   - Never returns a value

## Still need to implement nonconst class method

def build_dict(class_name, function, return_type, args, pointer, method=False):
    if pointer:
        arg1 = [('Rcpp::RObject', 'x', 'ptr')]
    else:
        arg1 = [('const forest::%s&' % class_name, 'tr')]
    args = arg1 + args
    call_args = args[1:] if method else args
    d = {'class_name': class_name,
         'function': function,
         'tree_type': 'tree' if class_name == "forest_tree" else "subtree",
         'definition': definition(args),
         'return_type': return_type,
         'return_statement': '' if return_type == 'void' else 'return ',
         'call': ('*' if pointer and not method else '') + call(call_args)}
    return d

def export_ptr(class_name, function, return_type='void', args=[]):
    template = """// [[Rcpp::export]]
${return_type} ${class_name}__${function}(${definition}) {
  Rcpp::XPtr<forest::${class_name}> ptr =
    forest::exporters::ptr_${tree_type}_from_R<forest::forest_node>(x);
  ${return_statement}forest::${function}(${call});
}
"""
    d = build_dict(class_name, function, return_type, args, True)
    cog.out(tp(template, d))

## Note that using this with 'void' is probably stupid.  I should
## probably give a warning here.
def export_cr(class_name, function, return_type='void', args=[]):
    template = """// [[Rcpp::export]]
${return_type} ${class_name}__${function}(${definition}) {
  ${return_statement}forest::${function}(${call});
}
"""
    d = build_dict(class_name, function, return_type, args, False)
    cog.out(tp(template, d))

def export_ptr_method(class_name, function, return_type='void', args=[]):
    template = """// [[Rcpp::export]]
${return_type} ${class_name}__${function}(${definition}) {
  Rcpp::XPtr<forest::${class_name}> ptr =
    forest::exporters::ptr_${tree_type}_from_R<forest::forest_node>(x);
  ${return_statement}ptr->${function}(${call});
}
"""
    d = build_dict(class_name, function, return_type, args, True, True)
    cog.out(tp(template, d))

def export_cr_method(class_name, function, return_type, args=[]):
    template = """// [[Rcpp::export]]
${return_type} ${class_name}__${function}(${definition}) {
  ${return_statement}tr.${function}(${call});
}
"""
    d = build_dict(class_name, function, return_type, args, False, True)
    cog.out(tp(template, d))
