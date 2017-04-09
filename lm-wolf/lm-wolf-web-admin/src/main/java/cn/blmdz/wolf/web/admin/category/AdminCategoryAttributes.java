package cn.blmdz.wolf.web.admin.category;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.category.dto.ExchangeIndexDto;
import cn.blmdz.wolf.category.model.CategoryAttribute;
import cn.blmdz.wolf.category.service.CategoryAttributeReadService;
import cn.blmdz.wolf.category.service.CategoryAttributeWriteService;

@RestController
@RequestMapping({"/api/attributes"})
public class AdminCategoryAttributes {
   private static final Logger log = LoggerFactory.getLogger(AdminCategoryAttributes.class);
   private final CategoryAttributeWriteService categoryAttributeWriteService;
   private final CategoryAttributeReadService categoryAttributeReadService;

   @Autowired
   public AdminCategoryAttributes(CategoryAttributeWriteService categoryAttributeWriteService, CategoryAttributeReadService categoryAttributeReadService) {
      this.categoryAttributeWriteService = categoryAttributeWriteService;
      this.categoryAttributeReadService = categoryAttributeReadService;
   }

   @RequestMapping(
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   public Long create(@RequestBody CategoryAttribute categoryAttribute) {
      Response<Long> r = this.categoryAttributeWriteService.create(categoryAttribute);
      if(!r.isSuccess()) {
         log.error("failed to create {}, error code:{}", categoryAttribute, r.getError());
         throw new JsonResponseException(r.getError());
      } else {
         return (Long)r.getResult();
      }
   }

   @RequestMapping(
      method = {RequestMethod.PUT},
      produces = {"application/json"}
   )
   public boolean update(@RequestBody CategoryAttribute categoryAttribute) {
      Response<Boolean> r = this.categoryAttributeWriteService.update(categoryAttribute);
      if(!r.isSuccess()) {
         log.error("failed to update {}, error code:{}", categoryAttribute, r.getError());
         throw new JsonResponseException(r.getError());
      } else {
         return ((Boolean)r.getResult()).booleanValue();
      }
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

   @RequestMapping(
      value = {"/{id}"},
      method = {RequestMethod.DELETE},
      produces = {"application/json"}
   )
   public boolean delete(@PathVariable("id") Long id) {
      Response<Boolean> r = this.categoryAttributeWriteService.delete(id);
      if(!r.isSuccess()) {
         log.error("failed to delete category attribute(id={}), error code:{}", id, r.getError());
         throw new JsonResponseException(r.getError());
      } else {
         return ((Boolean)r.getResult()).booleanValue();
      }
   }

   @RequestMapping(
      value = {"/exchange"},
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   public boolean exchangeIndex(@RequestBody ExchangeIndexDto exchangeIndexDto) {
      Response<Boolean> r = this.categoryAttributeWriteService.exchangeIndex(exchangeIndexDto);
      if(!r.isSuccess()) {
         log.error("failed to exchange category attribute index for(id1={}, id2={}), error code:{}", new Object[]{exchangeIndexDto.getAttributeId1(), exchangeIndexDto.getAttributeId2(), r.getError()});
         throw new JsonResponseException(r.getError());
      } else {
         return ((Boolean)r.getResult()).booleanValue();
      }
   }
}
