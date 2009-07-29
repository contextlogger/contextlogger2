import _cl2_client as native

class ScriptError(Exception):
    pass

class Session:

    def __init__(self):
        """
        Constructs a session. After finishing using a instance, you
        should call close() on it to free the associated resources.
        """
        self.session = native.Session()

    def eval(self, expr):
        """
        Evaluates a Lua expression on the server side. The result is
        converted to textual form and returned, or an exception is
        thrown if the expression is not valid or otherwise fails to
        evaluate.
        """
        # TODO: check for buffer overflow error and retrieve result in multiple parts if required -- not quite supported on native side yet either
        evalErr, text = self.session.eval_get_result(unicode(expr))
        if evalErr == 0:
            return text
        else:
            raise ScriptError, ("Error evaluating expression \"%s\": %s (Symbian error %d)" % (expr, text, evalErr))

    def close(self):
        """
        Frees the resources associated with the object instance. The
        object may no longer be used after this method has been
        called. It is okay to call this method more than once, though.
        """
        self.session.close()

