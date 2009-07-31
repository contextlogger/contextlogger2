class Array
  def <(x)
    (self <=> x) < 0
  end

  def >(x)
    (self <=> x) > 0
  end

  def <=(x)
    (self <=> x) <= 0
  end

  def >=(x)
    (self <=> x) >= 0
  end
end

