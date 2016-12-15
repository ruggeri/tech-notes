So to tell Slim to render fenced code blocks, you need to configure
Redcarpet(the markdown renderer):

```
Slim::Engine.set_options(
  :markdown => {
    fenced_code_blocks: true,
  }
)
```

Then, further, if you want to use Rouge to do syntax highlighting, you
need to extend the HTML rendering engine:

```
require 'redcarpet'
require 'rouge'
require 'rouge/plugins/redcarpet'
require 'slim'

# Not sure why I can't just extend Redcarpet::Render::HTML
class MyHTML < Redcarpet::Render::HTML
  # This is a plugin to swap out the code_block rendering code
  # and wrap it with Rouge stuff.
  include Rouge::Plugins::Redcarpet
end

# This is how we configure Slim's use of redcarpet.
Slim::Engine.set_options(
  :markdown => {
    fenced_code_blocks: true,
    renderer: MyHTML.new(render_options = {})
  }
)
```

See how we've told Slim to use our extended engine?

Now download a stylesheet from pygments-css and add it to your vendor
stylesheets. Weird, you have to add this to your MyHTML:

```
  def rouge_formatter(lexer)
    Rouge::Formatters::HTMLLegacy.new(:css_class => "codehilite #{lexer.tag}")
  end
```

The change is that Rouge uses `highlight` but pygments-css needs
`codehilite`. Otherwise this method is just a copy.

The background color may not render properly, so you might have to add
that. Odd.

I liked github.css pretty well, which looks nice on a mostly white
background site.
