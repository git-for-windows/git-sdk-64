class AttrDict(dict):
    """
    Like a dictionary except values can be accessed as attributes i.e. obj.foo
    can be used in addition to obj['foo'].
    If an item is not present None is returned.
    """
    def __getattr__(self, key):
        try:
            return self[key]
        except KeyError:
            return None

    def __setattr__(self, key, value):
        self[key] = value

    def __delattr__(self, key):
        try:
            del self[key]
        except KeyError as k:
            raise AttributeError(k)

    def __repr__(self):
        return '<AttrDict ' + dict.__repr__(self) + '>'

    def __getstate__(self):
        return dict(self)

    def __setstate__(self, value):
        for k, v in value.items():
            self[k] = v


class DefaultAttrDict(AttrDict):
    def __getattr__(self, key):
        try:
            return self[key]
        except KeyError as k:
            if '_default' in self:
                return self['_default']
            else:
                raise AttributeError from k


class InsensitiveDict(dict):
    """
    Like a dictionary except key access is case insensitive.
    Keys are stored in lower case.
    """
    def __getitem__(self, key):
        return dict.__getitem__(self, key.lower())

    def __setitem__(self, key, value):
        dict.__setitem__(self, key.lower(), value)

    def __delitem__(self, key):
        dict.__delitem__(self, key.lower())

    def get(self, key, default=None):
        return dict.get(self, key.lower(), default)

    def update(self, dict):
        for k, v in dict.items():
            self[k] = v

    def setdefault(self, key, default=None):
        return dict.setdefault(self, key.lower(), default)

    def __contains__(self, key):
        return key.lower() in dict(self)
