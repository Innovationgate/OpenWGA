module Sass::Script::Functions
  def wga_css_url(name)
    Sass::Script::Value::String.new(Java::DeInnovationgateWgaAdditional_script_langsSass::SassFunctions.wga_css_url(name.value, options))
  end
  
  def wga_file_url(db=nil, container, name)
    if db.nil?
    	Sass::Script::Value::String.new(Java::DeInnovationgateWgaAdditional_script_langsSass::SassFunctions.wga_file_url(nil, container.value, name.value, options))
    else
    	Sass::Script::Value::String.new(Java::DeInnovationgateWgaAdditional_script_langsSass::SassFunctions.wga_file_url(db.value, container.value, name.value, options))
    end
  end
  
  declare :wga_css_url, [:name]
  declare :wga_file_url, [:db, :container, :name]
end
