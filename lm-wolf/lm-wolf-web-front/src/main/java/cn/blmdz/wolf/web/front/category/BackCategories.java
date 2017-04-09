package cn.blmdz.wolf.web.front.category;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.wolf.cache.BackCategoryCacher;

@RestController
@RequestMapping({"/api/backCategories"})
public class BackCategories {
   private static final Logger log = LoggerFactory.getLogger(BackCategories.class);
   private final BackCategoryCacher backCategoryCacher;

   @Autowired
   public BackCategories(BackCategoryCacher backCategoryCacher) {
      this.backCategoryCacher = backCategoryCacher;
   }

   @RequestMapping(
      value = {"/children"},
      method = {RequestMethod.GET},
      produces = {"application/json"}
   )
   public List findChildrenByPid(@RequestParam(
   value = "pid",
   defaultValue = "0"
) Long pid) {
      try {
         return this.backCategoryCacher.findChildrenOf(pid);
      } catch (Exception var3) {
         log.error("failed to find back category children of category(id={}), cause:{}", pid, Throwables.getStackTraceAsString(var3));
         throw new JsonResponseException("category.children.find.fail");
      }
   }
}
