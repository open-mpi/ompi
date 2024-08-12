"""Source parsing code."""

class Parameter:

    def __init__(self, text, type_constructor):
        """Parse a parameter."""
        # parameter in the form "TYPE NAME" or "TYPE NAME:COUNT_VAR"
        type_name, namecount = text.split()
        if ':' in namecount:
            name, count_param = namecount.split(':')
        else:
            name, count_param = namecount, None
        self.type_name = type_name
        self.name = name
        self.count_param = count_param
        self.type_constructor = type_constructor

    def construct(self, **kwargs):
        """Construct the type parameter for the given ABI."""
        return self.type_constructor(type_name=self.type_name, name=self.name,
                                     count_param=self.count_param, **kwargs)


class ReturnType:
    """Return type wrapper."""

    def __init__(self, type_name, type_constructor):
        self.type_name = type_name
        self.type_constructor = type_constructor

    def construct(self, **kwargs):
        """Construct the return type for the given ABI."""
        return self.type_constructor(type_name=self.type_name, **kwargs)


class Prototype:
    """MPI function prototype."""

    def __init__(self, name, return_type, params):
        self.name = name
        self.return_type = return_type
        self.params = params

    def signature(self, fn_name, enable_count=False, **kwargs):
        """Build a signature with the given name and if count is enabled."""
        params = ', '.join(param.construct(**kwargs).parameter(enable_count=enable_count, **kwargs)
                           for param in self.params)
        if not params:
            params = 'void'
        return_type_text = self.return_type.construct(**kwargs).type_text(enable_count=enable_count)
        return f'{return_type_text} {fn_name}({params})'


def validate_body(body):
    """Validate the body of a template."""
    # Just do a simple bracket balance test to determine the bounds of the
    # function body. All lines after the function body should be blank. There
    # are cases where this will break, such as if someone puts code all on one
    # line.
    bracket_balance = 0
    line_count = 0
    for line in body:
        line = line.strip()
        if bracket_balance == 0 and line_count > 0 and line:
            raise util.BindingError('Extra code found in template; only one function body is allowed')

        update = line.count('{') - line.count('}')
        bracket_balance += update
        if bracket_balance != 0:
            line_count += 1

    if bracket_balance != 0:
        raise util.BindingError('Mismatched brackets found in template')


class SourceTemplate:
    """Source template for a single API function."""

    def __init__(self, prototype, header, body):
        self.prototype = prototype
        self.header = header
        self.body = body

    @staticmethod
    def load(fname, prefix=None, type_constructor=None):
        """Load a template file and return the SourceTemplate."""
        if prefix is not None:
            fname = os.path.join(prefix, fname)
        with open(fname) as fp:
            header = []
            prototype = []
            body = []

            for line in fp:
                line = line.rstrip()
                if prototype and line.startswith('PROTOTYPE'):
                    raise util.BindingError('more than one prototype found in template file')
                elif ((prototype and not any(')' in s for s in prototype))
                      or line.startswith('PROTOTYPE')):
                    prototype.append(line)
                elif prototype:
                    # Validate bracket balance
                    body.append(line)
                else:
                    header.append(line)

            if not prototype:
                raise RuntimeError(f'missing prototype for {fname}')
            # Parse the prototype
            prototype = ''.join(prototype)
            prototype = prototype[len('PROTOTYPE'):]
            i = prototype.index('(')
            j = prototype.index(')')
            return_type, name = prototype[:i].split()
            return_type = ReturnType(return_type, type_constructor=type_constructor)
            params = [param.strip() for param in prototype[i + 1:j].split(',') if param.strip()]
            params = [Parameter(param, type_constructor=type_constructor) for param in params]
            prototype = Prototype(name, return_type, params)
            # Ensure the body contains only one function
            validate_body(body)
            return SourceTemplate(prototype, header, body)

    def print_header(self, out):
        """Print the source header."""
        for line in self.header:
            out.dump(line)

    def print_body(self, func_name, out, replacements=None):
        """Print the body."""
        replacements = {} if replacements is None else replacements
        for line in self.body:
            # FUNC_NAME is used for error messages
            line = line.replace('FUNC_NAME', f'"{func_name}"')
            # Replace other parts in the body of the form '@KEY_NAME@'
            for key, value in replacements.items():
                line = line.replace(f'@{key}@', value)
            out.dump(line)
