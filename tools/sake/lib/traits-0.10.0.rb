module Traits
#--{{{
  VERSION = "0.10.0" 
  DEBUG = ENV['__TRAIT_DEBUG__'] || ENV['TRAIT_DEBUG'] || ENV['DEBUG']
#--}}}
end

class Object
#--{{{
  def singleton_method_added(*a, &b)
#--{{{
    ret = super rescue nil
    obj = self
    obj.__trait_singleton_class.__trait_module_eval{ @__trait_singleton_super = obj }
    ret
#--}}}
  end
  def __trait_singleton_class
#--{{{
    @__trait_singleton_class ||= class << self;self;end
#--}}}
  end
  def __trait_singleton?
#--{{{
    @__is_trait_singleton ||= 
      ((Class === self) and (@__is_trait_singleton ||= (not self.ancestors.include?(self))))
#--}}}
  end
  def __trait_search_path
#--{{{
    #((Class === self and not __trait_singleton?) ? ancestors : [self])
    (__trait_singleton? or not respond_to? 'ancestors') ? [self] : ancestors
#--}}}
  end
  def __trait_getopt opts, key, default = nil
#--{{{
    return opts.delete(key) if opts.has_key? key

    key = "#{ key }"
    return opts.delete(key) if opts.has_key? key
    key = key.intern
    return opts.delete(key) if opts.has_key? key

    key = "#{ key }s"
    return opts.delete(key) if opts.has_key? key
    key = key.intern
    return opts.delete(key) if opts.has_key? key

    key = "#{ key }es"
    return opts.delete(key) if opts.has_key? key
    key = key.intern
    return opts.delete(key) if opts.has_key? key

    return default
#--}}}
  end
  def __trait_arg_filter(*args, &block)
#--{{{
    pre = nil
    post = nil
    validate = nil
    cast = nil
    munge = nil
    type = nil
    ducktype = nil
    default = nil
    names_and_defaults = nil
    toggle = nil

    if block and not block.respond_to? :__trait_default
      block.__trait_singleton_class.class_eval{ attr '__trait_default' }
    end

    list = [ args ].flatten.dup
    opts = Hash::new.update(((list.size > 1 and Hash === list.last) ? list.pop : {}))

    pre = __trait_getopt opts, 'pre'
    post = __trait_getopt opts, 'post'
    validate = __trait_getopt opts, 'validate'
    cast = __trait_getopt opts, 'cast'
    munge = __trait_getopt opts, 'munge'
    type = __trait_getopt(opts, 'type', __trait_getopt(opts, 'case'))
    ducktype = __trait_getopt opts, 'ducktype'
    default = __trait_getopt opts, 'default'
    toggle = __trait_getopt opts, 'toggle'

    list, hashes = list.partition{|arg| not Hash === arg}
    hashes << opts unless opts.empty? # in which case it was not, in fact, opts

    names_and_defaults = hashes.inject({}){|h,h2| h.update h2} 

    raise ArgumentError, 
      "cannot specify both 'default' keyword and 'default' block" if
        block and default

    default ||= block

    # force list and names_and_defaults.keys to strings
    list = list.map{|t| "#{ t }"}
    #names_and_defaults = Hash[ *names_and_defaults.to_a.map{|k,v| ["#{ k }", v]}.flatten ]
    h = names_and_defaults
    h.keys.each{|k| h[k.to_s] = h.delete(k)}

    list.each{|name| names_and_defaults[name] = default}

    names = list + (names_and_defaults.keys - list)

    hooks = {
      'pre' => pre, 
      'cast' => cast,
      'munge' => munge, 
      'type' => type,
      'ducktype' => ducktype,
      'validate' => validate, 
      'post' => post, 
      'toggle' => toggle,
    }

    names_and_hooks = names.inject({}){|h, name| h.update name => hooks}

    ret = {
      'names_and_defaults' => names_and_defaults,
      'names_and_hooks' => names_and_hooks,
    }
#--}}}
  end
  def __trait_instance_method_list
#--{{{
    @__trait_instance_method_list ||= {'writers' => [], 'readers' => [], }
#--}}}
  end
  def __trait_singleton_method_list
#--{{{
    @__trait_singleton_method_list ||= {'writers' => [], 'readers' => [], }
#--}}}
  end
  def __trait_instance_method_defaults
#--{{{
      @__trait_instance_method_defaults ||= {}
#--}}}
  end
  def __trait_singleton_method_defaults
#--{{{
      @__trait_singleton_method_defaults ||= {} 
#--}}}
  end
  def __trait_instance_method_hooks
#--{{{
      @__trait_instance_method_hooks ||= 
        #Hash::new{ |h,name| h[name] = {'pre' => nil, 'post' => nil, 'validate' => nil, 'munge' => nil, 'cast' => nil} }
        Hash::new{ |h,name| h[name] = {} }
#--}}}
  end
  def __trait_singleton_method_hooks
#--{{{
      @__trait_singleton_method_hooks ||= 
        #Hash::new{ |h,name| h[name] = {'pre' => nil, 'post' => nil, 'validate' => nil, 'munge' => nil, 'cast' => nil} }
        Hash::new{ |h,name| h[name] = {} }
#--}}}
  end
  def __trait_evaluate(*a, &b)
#--{{{
    m = "__trait_evaluate__#{ Thread::current.object_id.abs }__#{ rand 42 }__#{ rand 666 }__"
    __trait_singleton_class.module_eval{ define_method m, &b }
    begin
      send m, *a
    ensure
      __trait_singleton_class.module_eval{ remove_method m }
    end
#--}}}
  end

  def __trait_define_singleton_reader_traits(*args, &block)
#--{{{
    argf = __trait_arg_filter args, &block
    defaults = __trait_singleton_method_defaults
    list = __trait_singleton_method_list 
    mhooks = __trait_singleton_method_hooks
    argf['names_and_defaults'].each do |name, default|
      __trait_singleton_class.__trait_define_reader_trait name, default, defaults, list
    end
    argf['names_and_hooks'].each{|name, hooks| mhooks[name].update hooks}
    __trait_search_path.map{|ta| ta.__trait_singleton_method_list['readers']}.flatten
#--}}}
  end
  def __trait_define_singleton_writer_traits(*args, &block)
#--{{{
    argf = __trait_arg_filter args, &block
    defaults = __trait_singleton_method_defaults
    list = __trait_singleton_method_list 
    mhooks = __trait_singleton_method_hooks
    argf['names_and_defaults'].each do |name, default|
      __trait_singleton_class.__trait_define_writer_trait name, default, defaults, list
    end
    argf['names_and_hooks'].each{|name, hooks| mhooks[name].update hooks}
    __trait_search_path.map{|ta| ta.__trait_singleton_method_list['writers']}.flatten
#--}}}
  end
  def __trait_define_singleton_traits(*args, &block)
#--{{{
    writers = __trait_define_singleton_writer_traits(*args, &block)
    readers = __trait_define_singleton_reader_traits(*args, &block)
    wr = writers.inject({}){|h,k| h.update k.delete('=') => k}
    readers.map{|r| [r, wr[r]]}
#--}}}
  end
  %w( 
      instance_reader_traits
      instance_reader_trait
      reader_traits
      reader_trait
      r_traits
      r_trait
      rtraits
      rtrait
      has_readers
      has_reader
      has_r

      instance_writer_traits
      instance_writer_trait
      writer_traits
      writer_trait
      w_traits
      w_trait
      wtraits
      wtrait
      has_writers
      has_writer
      has_w

      instance_traits
      instance_trait
      traits
      trait
      has
    ).each do |meth|
      eval <<-def
        def #{ meth }(*a, &b)
          __trait_singleton_class.send('#{ meth }', *a, &b)
        end
      def
    end
#--}}}
end

class Class
#--{{{
  def __trait_singleton_super
#--{{{
    class_eval{ def __________trait_singleton_super_init;end } unless defined? @__trait_singleton_super
    @__trait_singleton_super
#--}}}
  end
#--}}}
end

class Module 
#--{{{

  def __trait_module_eval(*a, &b)
#--{{{
    begin
      module_eval(*a, &b)
    rescue Exception => e
      STDERR.puts([a, b].inspect) if Traits::DEBUG
      raise
    end
#--}}}
  end

  def __trait_define_instance_reader_traits(*args, &block)
#--{{{
    if __trait_singleton?
      return(__trait_singleton_super.__trait_define_singleton_reader_traits(*args, &block))
    end
    argf = __trait_arg_filter args, &block
    defaults = __trait_instance_method_defaults
    list = __trait_instance_method_list 
    mhooks = __trait_instance_method_hooks
    argf['names_and_defaults'].each do |name, default|
      __trait_define_reader_trait name, default, defaults, list
    end
    argf['names_and_hooks'].each{|name, hooks| mhooks[name].update hooks}
    __trait_search_path.map{|ta| ta.__trait_instance_method_list['readers']}.flatten
#--}}}
  end
  %w( 
      instance_reader_traits
      instance_reader_trait
      reader_traits
      reader_trait
      rtraits
      rtrait
      r_traits
      r_trait
      has_readers
      has_reader
      has_r
    ).each{|meth| alias_method meth, '__trait_define_instance_reader_traits'}

  def __trait_define_instance_writer_traits(*args, &block)
#--{{{
    if __trait_singleton?
      return(__trait_singleton_super.__trait_define_singleton_writer_traits(*args, &block))
    end
    argf = __trait_arg_filter args, &block
    defaults = __trait_instance_method_defaults
    list = __trait_instance_method_list 
    mhooks = __trait_instance_method_hooks
    argf['names_and_defaults'].each do |name, default|
      __trait_define_writer_trait name, default, defaults, list
    end
    argf['names_and_hooks'].each{|name, hooks| mhooks[name].update hooks}
    __trait_search_path.map{|ta| ta.__trait_instance_method_list['writers']}.flatten
#--}}}
  end
  %w( 
      instance_writer_traits
      instance_writer_trait
      writer_traits
      writer_trait
      wtraits
      wtrait
      w_traits
      w_trait
      has_writers
      has_writer
      has_w
    ).each{|meth| alias_method meth, '__trait_define_instance_writer_traits'}

  def __trait_define_instance_traits(*args, &block)
#--{{{
    writers = __trait_define_instance_writer_traits(*args, &block)
    readers = __trait_define_instance_reader_traits(*args, &block)
    #[readers, writers]
    wr = writers.inject({}){|h,k| h.update k.delete('=') => k}
    readers.map{|r| [r, wr[r]]}
#--}}}
  end
  %w( 
      instance_traits
      instance_trait
      traits
      trait
      has
    ).each{|meth| alias_method meth, '__trait_define_instance_traits'}

  def __trait_define_class_reader_traits(*args, &block)
#--{{{
    __trait_define_singleton_reader_traits(*args, &block)
#--}}}
  end
  %w( 
      class_reader_traits
      class_reader_trait
      class_rtraits
      class_rtrait
      class_r_traits
      class_r_trait
      class_has_readers
      class_has_reader
      class_has_r
      c_has_readers
      c_has_reader
      c_has_r
    ).each{|meth| alias_method meth, '__trait_define_class_reader_traits'}

  def __trait_define_class_writer_traits(*args, &block)
#--{{{
    __trait_define_singleton_writer_traits(*args, &block)
#--}}}
  end
  %w( 
      class_writer_traits
      class_writer_trait
      class_wtraits
      class_wtrait
      class_w_traits
      class_w_trait
      class_has_writers
      class_has_writer
      class_has_w
      c_has_writers
      c_has_writer
      c_has_w
    ).each{|meth| alias_method meth, '__trait_define_class_writer_traits'}

  def __trait_define_class_traits(*args, &block)
#--{{{
    __trait_define_singleton_traits(*args, &block)
#--}}}
  end
  %w( 
      class_traits
      class_trait
      class_has
      c_has
    ).each{|meth| alias_method meth, '__trait_define_class_traits'}

  def __trait_define_reader_trait name, default, defaults, list
#--{{{
      getter = "#{ name }"
      setter = "#{ name }="
      query = "#{ name }?"
      toggle = "#{ name }!"

      defaults[getter] = default if default
      list['readers'] << getter 
      list['readers'].uniq!
      # list['readers'].sort!

      unless instance_methods.include? getter
        code = __trait_gen_reader_code name, 'public'
        __trait_module_eval code
      end
      unless instance_methods.include? setter
        code = __trait_gen_writer_code name, 'private'
        __trait_module_eval code
      end
      unless instance_methods.include? query
        code = __trait_gen_query_code name, 'public'
        __trait_module_eval code
      end
      unless instance_methods.include? toggle
        code = __trait_gen_toggle_code name, 'private'
        __trait_module_eval code
      end
#--}}}
  end

  def __trait_define_writer_trait name, default, defaults, list
#--{{{
      reader = "#{ name }"
      writer = "#{ name }="
      query = "#{ name }?"
      toggle = "#{ name }!"

      defaults[reader] = default if default
      list['writers'] << writer 
      list['writers'].uniq!
      # list['writers'].sort!

      unless instance_methods.include? reader
        code = __trait_gen_reader_code name, 'private'
        __trait_module_eval code
      end
      unless instance_methods.include? writer
        code = __trait_gen_writer_code name, 'public'
        __trait_module_eval code
      end
      unless instance_methods.include? query
        code = __trait_gen_query_code name, 'private'
        __trait_module_eval code
      end
      unless instance_methods.include? toggle
        code = __trait_gen_toggle_code name, 'public'
        __trait_module_eval code
      end
#--}}}
  end

  def __trait_gen_reader_code name, access_protection = 'public' 
#--{{{
    s = __trait_singleton?
    klass = s ? 'self' : 'self.class'
    defaults = s ? '__trait_singleton_method_defaults' : '__trait_instance_method_defaults'
    search_ancestors = s ? 'true' : 'false'

    getter_meth = "#{ name }"
    setter_meth = "#{ name }="
    query_meth = "#{ name }?"
    ivar = "@#{ name }"
    getter = "'#{ name }'"
    setter = "'#{ name }='"

    reader_code = <<-code
      def #{ name }(*a, &b)
        unless a.empty?
          send('#{ name }=', *a, &b)
        else
          #unless(defined?(@________#{ name }_set) and @________#{ name }_set)
          unless defined? @#{ name }
            #{ klass }::__trait_search_path.each do |obj|
                defaults = obj.#{ defaults }
                if defaults.has_key? '#{ name }'
                  d = defaults['#{ name }']
                  if d.respond_to? '__trait_default'
                    d = instance_eval &d
                  end
                  return(send('#{ name }=', d))
                end
            end
          end
          case self
            when Class
              if defined? @#{ name }
                @#{ name }
              else
                __trait_search_path.each do |obj|
                  if self != obj and obj.respond_to? '#{ name }'
                    return(obj.send('#{ name }', *a,  &b))
                  end
                end
                return nil
              end
            else
              defined? @#{ name } and @#{ name }
          end
        end
      end
      #{ access_protection } '#{ name }'.intern
    code
    puts reader_code if Traits::DEBUG
    reader_code
#--}}}
  end

  def __trait_gen_writer_code name, access_protection = 'public' 
#--{{{
    s = __trait_singleton?
    klass = s ? 'self' : 'self.class'
    hooks = s ? '__trait_singleton_method_hooks' : '__trait_instance_method_hooks'

    writer_code = <<-code
      def #{ name }=(head, *tail)
        value = tail.empty? ? head : [head] + tail

        hooks = {}
        hook_types = %w( pre munge cast type ducktype validate post )

        #{ klass }::__trait_search_path.each do |obj|

          break if hooks.values_at(*hook_types).compact.size == hook_types.size

          hook_types.each{|ht| hooks[ht] ||= obj.#{ hooks }['#{ name }'][ht]}

        end

        pre_hook, munge_hook, cast_hook, type_hook, ducktype_hook, validate_hook, post_hook = 
          hooks.values_at(*hook_types) 

        if pre_hook
          [ pre_hook ].flatten.each do |pre|
            if Proc === pre 
              case pre.arity
                when 0
                  __trait_evaluate &pre
                when 1
                  __trait_evaluate value, &pre
                else
                  __trait_evaluate "#{ name }", value, &pre
              end
            else
              case method("\#{ pre }").arity
                when 0
                  send "\#{ pre }"
                when 1
                  send "\#{ pre }", value
                else
                  send "\#{ pre }", "#{ name }", value
              end
            end
          end
        end

        if cast_hook
          [ cast_hook ].flatten.each do |cast|
            value =
              if Proc === cast
                __trait_evaluate value, &cast
              else
                send cast, value
              end
          end
        end

        if munge_hook
          [ munge_hook ].flatten.each do |munge|
            value =
              if Proc === munge
                __trait_evaluate value, &munge
              else
                value.send munge
              end
          end
        end

        if type_hook
          [ type_hook ].flatten.each do |type|
            is_valid = 
              if Proc === type
                __trait_evaluate(value, &type) === value
              else
                type === value
              end
            raise ArgumentError, 
              "validation of <\#{ value.inspect }> failed!" unless is_valid
          end
        end

        if ducktype_hook
          [ ducktype_hook ].flatten.each do |ducktype|
            is_valid = 
              if Proc === ducktype
                value.respond_to? __trait_evaluate(value, &ducktype)
              else
                value.respond_to? ducktype
              end
            raise ArgumentError, 
              "validation of <\#{ value.inspect }> failed!" unless is_valid
          end
        end

        if validate_hook
          [ validate_hook ].flatten.each do |validate|
            is_valid = 
              if Proc === validate
                __trait_evaluate value, &validate
              else
                send "\#{ validate }", value
              end
            raise ArgumentError, 
              "validation of <\#{ value.inspect }> failed!" unless is_valid
          end
        end

        @#{ name } = value 

        #@________#{ name }_set = true 

        if post_hook
          [ post_hook ].flatten.each do |post|
            if Proc === post 
              case post.arity
                when 0
                  __trait_evaluate &post
                when 1
                  __trait_evaluate value, &post
                else
                  __trait_evaluate "#{ name }", value, &post
              end
            else
              case method("\#{ post }").arity
                when 0
                  send "\#{ post }"
                when 1
                  send "\#{ post }", value
                else
                  send "\#{ post }", "#{ name }", value
              end
            end
          end
        end

        @#{ name }
      end
      #{ access_protection } '#{ name }='.intern
    code
    puts writer_code if Traits::DEBUG
    writer_code
#--}}}
  end

  def __trait_gen_query_code name, access_protection = 'public' 
#--{{{
    query_code = <<-code
      def #{ name }?
        send('#{ name }') ? true : false
      end
      # #{ access_protection } '#{ name }?'.intern
    code
    puts query_code if Traits::DEBUG
    query_code
#--}}}
  end

  def __trait_gen_toggle_code name, access_protection = 'public' 
#--{{{
    s = __trait_singleton?
    klass = s ? 'self' : 'self.class'
    hooks = s ? '__trait_singleton_method_hooks' : '__trait_instance_method_hooks'

    writer_code = <<-code
      def #{ name }!
        hooks = {}
        hook_types = %w( pre munge cast type ducktype validate post toggle )

        #{ klass }::__trait_search_path.each do |obj|

          break if hooks.values_at(*hook_types).compact.size == hook_types.size

          hook_types.each{|ht| hooks[ht] ||= obj.#{ hooks }['#{ name }'][ht]}

        end

        pre_hook, munge_hook, cast_hook, type_hook, ducktype_hook, validate_hook, post_hook, toggle_hook = 
          hooks.values_at(*hook_types) 

        toggle =
          if defined? @________#{ name }_toggle
            @________#{ name }_toggle
          else
            if toggle_hook
              case toggle_hook
                when Proc
                  t = Object.new
                  sc = class << t 
                    self
                  end
                  sc.module_eval{
                    define_method 'shift' do
                      toggle_hook.call
                    end
                    define_method 'push' do
                    end
                  }
                  t
                when Enumerable
                  toggle_hook.to_a
                when NilClass, TrueClass 
                  @________#{ name }_toggle = [true, false] 
                else
                  if toggle_hook.respond_to?('shift') and toggle_hook.respond_to?('push')
                    toggle_hook
                  else
                    raise "bad toggle <\#{ toggle_hook.inspect }>"
                  end
              end
            else
              @________#{ name }_toggle = [true, false] 
            end
          end

        value = toggle.shift
        toggle.push value

        if pre_hook
          [ pre_hook ].flatten.each do |pre|
            if Proc === pre 
              case pre.arity
                when 0
                  __trait_evaluate &pre
                when 1
                  __trait_evaluate value, &pre
                else
                  __trait_evaluate "#{ name }", value, &pre
              end
            else
              case method("\#{ pre }").arity
                when 0
                  send "\#{ pre }"
                when 1
                  send "\#{ pre }", value
                else
                  send "\#{ pre }", "#{ name }", value
              end
            end
          end
        end

        if cast_hook
          [ cast_hook ].flatten.each do |cast|
            value =
              if Proc === cast
                __trait_evaluate value, &cast
              else
                send cast, value
              end
          end
        end

        if munge_hook
          [ munge_hook ].flatten.each do |munge|
            value =
              if Proc === munge
                __trait_evaluate value, &munge
              else
                value.send munge
              end
          end
        end

        if type_hook
          [ type_hook ].flatten.each do |type|
            is_valid = 
              if Proc === type
                __trait_evaluate(value, &type) === value
              else
                type === value
              end
            raise ArgumentError, 
              "validation of <\#{ value.inspect }> failed!" unless is_valid
          end
        end

        if ducktype_hook
          [ ducktype_hook ].flatten.each do |ducktype|
            is_valid = 
              if Proc === ducktype
                value.respond_to? __trait_evaluate(value, &ducktype)
              else
                value.respond_to? ducktype
              end
            raise ArgumentError, 
              "validation of <\#{ value.inspect }> failed!" unless is_valid
          end
        end

        if validate_hook
          [ validate_hook ].flatten.each do |validate|
            is_valid = 
              if Proc === validate
                __trait_evaluate value, &validate
              else
                send "\#{ validate }", value
              end
            raise ArgumentError, 
              "validation of <\#{ value.inspect }> failed!" unless is_valid
          end
        end

        @#{ name } = value 

        #@________#{ name }_set = true 

        if post_hook
          [ post_hook ].flatten.each do |post|
            if Proc === post 
              case post.arity
                when 0
                  __trait_evaluate &post
                when 1
                  __trait_evaluate value, &post
                else
                  __trait_evaluate "#{ name }", value, &post
              end
            else
              case method("\#{ post }").arity
                when 0
                  send "\#{ post }"
                when 1
                  send "\#{ post }", value
                else
                  send "\#{ post }", "#{ name }", value
              end
            end
          end
        end

        @#{ name }
      end
      #{ access_protection } '#{ name }='.intern
    code
    puts writer_code if Traits::DEBUG
    writer_code
#--}}}
  end

  def __trait_gen_access_protection_code name, access_protection = nil 
#--{{{
    access_protection ||= 'public' 
    access_protection_code =
      case access_protection
        when %r/private/
          "private '#{ name }'.intern"
        when %r/protected/
          "protected '#{ name }'.intern"
        else
          "public '#{ name }'.intern"
      end
    puts access_protection_code if Traits::DEBUG
    access_protection_code
#--}}}
  end
#--}}}
end

module TraitInit
#--{{{
  module InstaceMethods
    def trait_init *argv
#--{{{
      args, opts = argv.partition{|arg| not Hash === arg}
      args.flatten!
      opts = opts.inject({}){|h,h2| h.update h2}
      msgs = self.class.r_traits
      args.each{|arg| send msgs.shift, arg}
      opts.each do |k,v| 
        k = "#{ k }"
        if respond_to? k
          send k, v
        else
          raise ArgumentError, "invalid trait -- #{ self.class }##{ k }"
        end
      end
#--}}}
    end
    alias_method "traitinit", "trait_init"
  end
  module ClassMethods
    def trait_initialize *a, &b
      traits *a unless a.empty?
      module_eval{
        def initialize(*a, &b)
          super() if defined? super
          trait_init *a
        end
      }
    end
    alias_method "traitinitialize", "trait_initialize"
  end
  def self.included other
#--{{{
    other.extend ClassMethods
    other.module_eval{ include InstaceMethods }
    super
#--}}}
  end
#--}}}
end

class OpenTraits 
#--{{{
  def initialize h = {}, &b
    h.each{|k,v| trait k => v}
    instance_eval &b if b
  end

  def method_missing m, *a, &b
    m = m.to_s.delete '='
    unless respond_to? m
      if a.empty?
        b ? trait(m, &b) : trait(m)
      else
        b ? trait(m => a.shift, &b) : trait(m => a.shift)
      end
    end
    send m
  end

  def to_hash
    rtraits.inject({}){|h,t| h.update t => send(t)}
  end
  alias_method "to_h", "to_hash"

  def to_s *a, &b
    to_hash.to_s *a, &b
  end

  def inspect *a, &b
    to_hash.inspect *a, &b
  end

  def to_yaml *a, &b
    to_hash.to_yaml *a, &b
  end
#--}}}
end
def OpenTraits(*a, &b) OpenTraits::new(*a, &b) end
def opentraits(*a, &b) OpenTraits::new(*a, &b) end
def otraits(*a, &b) OpenTraits::new(*a, &b) end
