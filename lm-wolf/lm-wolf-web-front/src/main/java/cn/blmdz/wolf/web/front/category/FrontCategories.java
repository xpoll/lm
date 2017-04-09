package cn.blmdz.wolf.web.front.category;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.category.model.FrontCategory;
import cn.blmdz.wolf.category.service.FrontCategoryReadService;

@RestController
@RequestMapping({"/api/frontCategories"})
public class FrontCategories {
   private static final Logger log = LoggerFactory.getLogger(FrontCategories.class);
   private final FrontCategoryReadService frontCategoryReadService;

   @Autowired
   public FrontCategories(FrontCategoryReadService frontCategoryReadService) {
      this.frontCategoryReadService = frontCategoryReadService;
   }

   @RequestMapping(
      value = {"/children"},
      method = {RequestMethod.GET},
      produces = {"application/json"}
   )
   public List findChildrenOfFrontend(@RequestParam(
   value = "pid",
   defaultValue = "0"
) Long pid) {
      Response<List<FrontCategory>> r = this.frontCategoryReadService.findChildrenByPid(pid);
      if(!r.isSuccess()) {
         log.warn("failed to find children of front category(id={}), error code:{}", pid, r.getError());
         throw new JsonResponseException(r.getError());
      } else {
         return (List)r.getResult();
      }
   }
}
