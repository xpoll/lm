package cn.blmdz.hunt.engine;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.base.Objects;
import com.google.common.base.Strings;

import cn.blmdz.hunt.engine.exception.NotFound404Exception;
import cn.blmdz.hunt.engine.handlebars.HandlebarEngine;

import java.io.FileNotFoundException;
import java.util.Map;

@Slf4j
@Component
public class PageRender {
    @Autowired
    protected HandlebarEngine handlebarEngine;

    /**
     * 按路径渲染一个页面，用于非装修页面的渲染
     */
    public String render(String domain, String path, Map<String, Object> context) {
        return naiveRender(path, context);
    }

    public String naiveRender(String path, Map<String, Object> context)
    	    throws NotFound404Exception
    	  {
    	    return naiveRender(path, null, context);
    	  }

    	  public String naiveRender(String templatePath, String shownPath, Map<String, Object> context)
    	    throws NotFound404Exception
    	  {
    	    context.put("_PATH_", Objects.firstNonNull(Strings.emptyToNull(shownPath), templatePath));
    	    try {
    	      return this.handlebarsEngine.execPath(templatePath, context, false);
    	    } catch (FileNotFoundException e) {
    	      log.error("failed to find page {},cause:{}", new Object[] { templatePath, e.getMessage(), e });
    	      throw new NotFound404Exception("page not found");
    	    }
    	  }
}