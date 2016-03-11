require 'fiber'

# You can always use an Enumerator in place of a Fiber, but it can be
# a lot harder to write, because you always need to "start at the top"
# of the next method.

class FibEnumerator
  def initialize
    @next_letter = false
    @cur, @next = 0, 1
  end

  def next
    if @next_letter
      @next_letter = false
      return "a"
    else
      @next_letter = true
    end

    rv, @cur, @next = @cur, @next, @cur + @next
    rv
  end
end

def FibFiber
  Fiber.new do |i|
    cur, nxt = 0, 1
    while true
      rv, cur, nxt = cur, nxt, cur + nxt

      Fiber.yield rv
      Fiber.yield "a"
    end
  end
end

# fe = FibEnumerator.new
ff = FibFiber()
100.times do |i|
  # p fe.next
  p ff.resume
end

# Fibers are an alternative to inversion of control through passing in
# an object. Here, the game yields back to ask the environment to do
# something for it.

def PlayGameFiber
  Fiber.new do
    puts "Hi, what is your name?"
    name = Fiber.yield([:string])

    puts "Hello #{name}, please give me your favorite color!"
    color = Fiber.yield([:string, ["red", "white", "blue"]])

    puts "How old are you, #{name}?"
    age = Fiber.yield([:integer])

    puts "Thanks for playing the program!"
  end
end

class InvalidOption; end

def run_io_code(fiber)
  req_type, options = fiber.resume
  while fiber.alive?
    case req_type
    when :string
      begin
        result = gets.chomp!
        if options && !options.include?(result)
          puts "Valid options are: #{options}"
          raise InvalidOption
        end
      rescue
        retry
      end
    when :integer
      begin
        puts "Give me an integer!"
        result = Integer(gets.chomp!)
      rescue
        puts "That was not an integer, my doge"
        retry
      end
    end

    req_type, options = fiber.resume(result)
  end
end

run_io_code(PlayGameFiber())
