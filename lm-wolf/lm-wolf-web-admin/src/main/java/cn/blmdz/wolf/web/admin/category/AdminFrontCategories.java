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
import cn.blmdz.wolf.category.model.FrontCategory;
import cn.blmdz.wolf.category.service.FrontCategoryReadService;
import cn.blmdz.wolf.category.service.FrontCategoryWriteService;

@RestController
@RequestMapping({"/api/frontCategories"})
public class AdminFrontCategories {
   private static final Logger log = LoggerFactory.getLogger(AdminFrontCategories.class);
   private final FrontCategoryReadService frontCategoryReadService;
   private final FrontCategoryWriteService frontCategoryWriteService;

   @Autowired
   public AdminFrontCategories(FrontCategoryReadService frontCategoryReadService, FrontCategoryWriteService frontCategoryWriteService) {
      this.frontCategoryReadService = frontCategoryReadService;
      this.frontCategoryWriteService = frontCategoryWriteService;
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

   @RequestMapping(
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   public FrontCategory create(@RequestBody FrontCategory frontCategory) {
      Response<FrontCategory> r = this.frontCategoryWriteService.create(frontCategory);
      if(!r.isSuccess()) {
         log.warn("failed to create {}, error code:{}", frontCategory, r.getError());
         throw new JsonResponseException(r.getError());
      } else {
         return (FrontCategory)r.getResult();
      }
   }

   @RequestMapping(
      value = {"/{id}/name"},
      method = {RequestMethod.PUT},
      produces = {"application/json"}
   )
   public boolean updateName(@PathVariable("id") long id, @RequestParam("name") String name) {
      Response<Boolean> r = this.frontCategoryWriteService.updateName(Long.valueOf(id), name);
      if(!r.isSuccess()) {
         log.warn("failed to update front category(id={}) name to {} ,error code:{}", new Object[]{Long.valueOf(id), name, r.getError()});
         throw new JsonResponseException(r.getError());
      } else {
         return ((Boolean)r.getResult()).booleanValue();
      }
   }

   @RequestMapping(
      value = {"/{id}/logo"},
      method = {RequestMethod.PUT},
      produces = {"application/json"}
   )
   public boolean updateLogo(@PathVariable("id") long id, @RequestParam("logo") String logo) {
      Response<Boolean> r = this.frontCategoryWriteService.updateLogo(Long.valueOf(id), logo);
      if(!r.isSuccess()) {
         log.warn("failed to update front category(id={}) logo to {} ,error code:{}", new Object[]{Long.valueOf(id), logo, r.getError()});
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
   public boolean delete(@PathVariable("id") Long id) {
      Response<Boolean> r = this.frontCategoryWriteService.delete(id);
      if(!r.isSuccess()) {
         log.warn("failed to delete front category(id={}) ,error code:{}", id, r.getError());
         throw new JsonResponseException(r.getError());
      } else {
         return ((Boolean)r.getResult()).booleanValue();
      }
   }
}
