class Session:
    """
    This describes the natively implemented interface.
    """

    def __init__(self):
        """
        Constructs a session. After finishing using a instance, you
        should call close() on it to free the associated resources.
        """
        pass

    def eval_get_result(self, expr):
        """
        Evaluates a Lua expression on the server side. An exception is
        thrown if there is an error in client-server comms. Otherwise
        the return value is a tuple (eval_err, result), where
        "eval_err" is a Symbian error code indicating whether the Lua
        expression evaluated okay, and "result" is either the result
        coerced to a string (when eval_err is 0), or a string
        describing the error (when eval_err is non-zero). "eval_err"
        will also be non-zero when the result is not coercable to a
        string.
        """
        pass

    def eval(self, expr):
        """
        Like "eval_get_result", but the return value is a tuple
        (eval_err, result_len), where "result_len" gives the length of
        "result" (as in "eval_get_result").
        """
        pass

    def get_result_length(self):
        """
        Returns an integer.
        """
        pass

    def get_result(self, offset, length):
        """
        Returns a Unicode string.
        """
        pass
        
    def close(self):
        """
        Frees the resources associated with the object instance. The
        object may no longer be used after this method has been
        called. It is okay to call this method more than once, though.
        """
        pass
