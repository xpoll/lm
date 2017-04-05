package io.terminus.parana.web.front.brand;

import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.Response;
import io.terminus.parana.brand.model.Brand;
import io.terminus.parana.brand.service.BrandReadService;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping({"/api/brands"})
public class Brands {
   private static final Logger log = LoggerFactory.getLogger(Brands.class);
   private final BrandReadService brandReadService;

   @Autowired
   public Brands(BrandReadService brandReadService) {
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
}
