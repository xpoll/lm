package cn.blmdz.wolf.web.admin.brand;

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
import cn.blmdz.wolf.brand.model.Brand;
import cn.blmdz.wolf.brand.service.BrandReadService;
import cn.blmdz.wolf.brand.service.BrandWriteService;

@RestController
@RequestMapping({"/api/brands"})
public class AdminBrands {
   private static final Logger log = LoggerFactory.getLogger(AdminBrands.class);
   private final BrandWriteService brandWriteService;
   private final BrandReadService brandReadService;

   @Autowired
   public AdminBrands(BrandWriteService brandWriteService, BrandReadService brandReadService) {
      this.brandWriteService = brandWriteService;
      this.brandReadService = brandReadService;
   }

   @RequestMapping(
      method = {RequestMethod.GET},
      produces = {"application/json"}
   )
   public List findByNamePrefix(@RequestParam(
   value = "name",
   required = false
) String namePrefix, @RequestParam(
   value = "count",
   defaultValue = "5"
) Integer count) {
      Response<List<Brand>> r = this.brandReadService.findByNamePrefix(namePrefix, count);
      if(!r.isSuccess()) {
         log.warn("failed to find brands by prefix({}), error code:{}", namePrefix, r.getError());
         throw new JsonResponseException(r.getError());
      } else {
         return (List)r.getResult();
      }
   }

   @RequestMapping(
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   public Long create(@RequestBody Brand brand) {
      Response<Long> r = this.brandWriteService.create(brand);
      if(!r.isSuccess()) {
         log.warn("failed to create {}, error code:{}", brand, r.getError());
         throw new JsonResponseException(r.getError());
      } else {
         return (Long)r.getResult();
      }
   }

   @RequestMapping(
      value = {"/{id}/logo"},
      method = {RequestMethod.PUT},
      produces = {"application/json"}
   )
   public Boolean updateLogo(@PathVariable Long id, @RequestParam String url) {
      Brand update = new Brand();
      update.setId(id);
      update.setLogo(url);
      Response<Boolean> tryUpdate = this.brandWriteService.update(update);
      if(!tryUpdate.isSuccess()) {
         log.error("failed to update {}, error code:{}", update, tryUpdate.getResult());
         throw new JsonResponseException(tryUpdate.getError());
      } else {
         return Boolean.TRUE;
      }
   }
}
