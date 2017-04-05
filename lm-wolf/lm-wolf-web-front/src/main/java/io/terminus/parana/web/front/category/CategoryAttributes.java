package io.terminus.parana.web.front.category;

import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.Response;
import io.terminus.parana.category.model.CategoryAttribute;
import io.terminus.parana.category.service.CategoryAttributeReadService;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping({"/api/attributes"})
public class CategoryAttributes {
   private static final Logger log = LoggerFactory.getLogger(CategoryAttributes.class);
   private final CategoryAttributeReadService categoryAttributeReadService;

   @Autowired
   public CategoryAttributes(CategoryAttributeReadService categoryAttributeReadService) {
      this.categoryAttributeReadService = categoryAttributeReadService;
   }

   @RequestMapping(
      method = {RequestMethod.GET},
      produces = {"application/json"}
   )
   public List findByCategoryId(@RequestParam("categoryId") Long categoryId) {
      Response<List<CategoryAttribute>> r = this.categoryAttributeReadService.findByCategoryId(categoryId);
      if(!r.isSuccess()) {
         log.error("failed to find category attributes for category(id={}), error code:{}", categoryId, r.getError());
         throw new JsonResponseException(r.getError());
      } else {
         return (List)r.getResult();
      }
   }
}
