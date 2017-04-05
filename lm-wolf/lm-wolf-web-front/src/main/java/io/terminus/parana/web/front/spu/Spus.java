package io.terminus.parana.web.front.spu;

import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.Paging;
import io.terminus.common.model.Response;
import io.terminus.parana.spu.model.Spu;
import io.terminus.parana.spu.service.SpuReadService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping({"/api/seller/spu"})
public class Spus {
   private static final Logger log = LoggerFactory.getLogger(Spus.class);
   private final SpuReadService spuReadService;

   @Autowired
   public Spus(SpuReadService spuReadService) {
      this.spuReadService = spuReadService;
   }

   @RequestMapping(
      value = {"/bycat"},
      method = {RequestMethod.GET},
      produces = {"application/json"}
   )
   public Paging findByCategoryId(@RequestParam(
   name = "categoryId"
) Long categoryId, @RequestParam(
   name = "keyword",
   required = false
) String keyword, @RequestParam(
   name = "pageNo",
   defaultValue = "1"
) Integer pageNo, @RequestParam(
   name = "pageSize",
   defaultValue = "10"
) Integer pageSize) {
      Response<Paging<Spu>> rSpu = this.spuReadService.findByCategoryId(categoryId, keyword, pageNo, pageSize);
      if(!rSpu.isSuccess()) {
         log.error("failed to find spu by category(id={}),error code:{}", categoryId, rSpu.getError());
         throw new JsonResponseException(rSpu.getError());
      } else {
         return (Paging)rSpu.getResult();
      }
   }
}
