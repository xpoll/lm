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
import cn.blmdz.wolf.parana.category.model.BackCategory;
import cn.blmdz.wolf.parana.category.service.CategoryBindingReadService;

@RestController
@RequestMapping({"/api/categoryBindings"})
public class CategoryBindings {
   private static final Logger log = LoggerFactory.getLogger(CategoryBindings.class);
   private final CategoryBindingReadService categoryBindingReadService;

   @Autowired
   public CategoryBindings(CategoryBindingReadService categoryBindingReadService) {
      this.categoryBindingReadService = categoryBindingReadService;
   }

   @RequestMapping(
      method = {RequestMethod.GET},
      produces = {"application/json"}
   )
   public List findByFrontCategoryId(@RequestParam("fid") Long frontCategoryId) {
      Response<List<BackCategory>> r = this.categoryBindingReadService.findByFrontCategoryId(frontCategoryId);
      if(!r.isSuccess()) {
         log.error("failed to find category bindings for front category(id={}),error code:{}", frontCategoryId, r.getError());
         throw new JsonResponseException(r.getError());
      } else {
         return (List)r.getResult();
      }
   }
}
