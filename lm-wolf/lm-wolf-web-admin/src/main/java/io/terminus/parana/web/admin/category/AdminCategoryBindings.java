package io.terminus.parana.web.admin.category;

import com.google.common.base.Joiner;
import com.google.common.collect.Lists;
import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.Response;
import io.terminus.parana.cache.BackCategoryCacher;
import io.terminus.parana.category.model.BackCategory;
import io.terminus.parana.category.service.CategoryBindingReadService;
import io.terminus.parana.category.service.CategoryBindingWriteService;
import java.io.Serializable;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping({"/api/categoryBindings"})
public class AdminCategoryBindings {
   private static final Logger log = LoggerFactory.getLogger(AdminCategoryBindings.class);
   private final CategoryBindingReadService categoryBindingReadService;
   private final CategoryBindingWriteService categoryBindingWriteService;
   private final BackCategoryCacher backCategoryCacher;

   @Autowired
   public AdminCategoryBindings(CategoryBindingReadService categoryBindingReadService, CategoryBindingWriteService categoryBindingWriteService, BackCategoryCacher backCategoryCacher) {
      this.categoryBindingReadService = categoryBindingReadService;
      this.categoryBindingWriteService = categoryBindingWriteService;
      this.backCategoryCacher = backCategoryCacher;
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
         List<BackCategory> backCategories = (List)r.getResult();
         List<AdminCategoryBindings.BindingShow> result = Lists.newArrayListWithCapacity(backCategories.size());

         for(BackCategory backCategory : backCategories) {
            List<BackCategory> ancestors = this.backCategoryCacher.findAncestorsOf(backCategory.getId());
            List<String> categoryNames = Lists.newArrayListWithCapacity(ancestors.size());

            for(BackCategory ancestor : ancestors) {
               categoryNames.add(ancestor.getName());
            }

            result.add(new AdminCategoryBindings.BindingShow(backCategory.getId(), Joiner.on('>').skipNulls().join(categoryNames)));
         }

         return result;
      }
   }

   @RequestMapping(
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   public Long bind(@RequestParam("fid") Long frontCategoryId, @RequestParam("bid") Long backCategoryId) {
      Response<Long> r = this.categoryBindingWriteService.bind(frontCategoryId, backCategoryId);
      if(!r.isSuccess()) {
         log.error("failed to bind front category(id={}) with back category(id={}),error code:{}", new Object[]{frontCategoryId, backCategoryId, r.getError()});
         throw new JsonResponseException(r.getError());
      } else {
         return (Long)r.getResult();
      }
   }

   @RequestMapping(
      value = {"/fid/{fid}/bid/{bid}"},
      method = {RequestMethod.DELETE},
      produces = {"application/json"}
   )
   public boolean unbind(@PathVariable("fid") Long frontCategoryId, @PathVariable("bid") Long backCategoryId) {
      Response<Boolean> r = this.categoryBindingWriteService.unBind(frontCategoryId, backCategoryId);
      if(!r.isSuccess()) {
         log.error("failed to unbind front category(id={}) with back category(id={}),error code:{}", new Object[]{frontCategoryId, backCategoryId, r.getError()});
         throw new JsonResponseException(r.getError());
      } else {
         return ((Boolean)r.getResult()).booleanValue();
      }
   }

   public static class BindingShow implements Serializable {
      private static final long serialVersionUID = -2901349425148529298L;
      private Long backCategoryId;
      private String path;

      public BindingShow(Long backCategoryId, String path) {
         this.backCategoryId = backCategoryId;
         this.path = path;
      }

      public Long getBackCategoryId() {
         return this.backCategoryId;
      }

      public String getPath() {
         return this.path;
      }
   }
}
