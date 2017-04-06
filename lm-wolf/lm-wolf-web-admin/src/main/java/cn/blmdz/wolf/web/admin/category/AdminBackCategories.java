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
import cn.blmdz.wolf.parana.category.model.BackCategory;
import cn.blmdz.wolf.parana.category.service.BackCategoryReadService;
import cn.blmdz.wolf.parana.category.service.BackCategoryWriteService;

@RestController
@RequestMapping({"/api/backCategories"})
public class AdminBackCategories {
   private static final Logger log = LoggerFactory.getLogger(AdminBackCategories.class);
   private final BackCategoryReadService backCategoryReadService;
   private final BackCategoryWriteService backCategoryWriteService;

   @Autowired
   public AdminBackCategories(BackCategoryReadService backCategoryReadService, BackCategoryWriteService backCategoryWriteService) {
      this.backCategoryReadService = backCategoryReadService;
      this.backCategoryWriteService = backCategoryWriteService;
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
      Response<List<BackCategory>> r = this.backCategoryReadService.findChildrenByPid(pid);
      if(!r.isSuccess()) {
         log.warn("failed to find children of back category(id={}), error code:{}", pid, r.getError());
         throw new JsonResponseException(r.getError());
      } else {
         return (List)r.getResult();
      }
   }

   @RequestMapping(
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   public BackCategory create(@RequestBody BackCategory backCategory) {
      Response<BackCategory> r = this.backCategoryWriteService.create(backCategory);
      if(!r.isSuccess()) {
         log.warn("failed to create {}, error code:{}", backCategory, r.getError());
         throw new JsonResponseException(r.getError());
      } else {
         return (BackCategory)r.getResult();
      }
   }

   @RequestMapping(
      value = {"/{id}/name"},
      method = {RequestMethod.PUT},
      produces = {"application/json"}
   )
   public boolean update(@PathVariable("id") long id, @RequestParam("name") String name) {
      Response<Boolean> r = this.backCategoryWriteService.updateName(Long.valueOf(id), name);
      if(!r.isSuccess()) {
         log.warn("failed to update back category(id={}) name to {} ,error code:{}", new Object[]{Long.valueOf(id), name, r.getError()});
         throw new JsonResponseException(r.getError());
      } else {
         return ((Boolean)r.getResult()).booleanValue();
      }
   }

   @RequestMapping(
      value = {"/{id}"},
      method = {RequestMethod.DELETE},
      produces = {"application/json"}
   )
   public boolean disable(@PathVariable("id") Long id) {
      Response<Boolean> r = this.backCategoryWriteService.disable(id);
      if(!r.isSuccess()) {
         log.warn("failed to disable back category(id={}) ,error code:{}", id, r.getError());
         throw new JsonResponseException(r.getError());
      } else {
         return ((Boolean)r.getResult()).booleanValue();
      }
   }
}
