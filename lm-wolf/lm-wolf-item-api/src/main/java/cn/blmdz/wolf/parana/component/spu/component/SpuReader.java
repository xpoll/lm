package cn.blmdz.wolf.parana.component.spu.component;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.base.Throwables;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.wolf.parana.component.dto.spu.EditSpu;
import cn.blmdz.wolf.parana.rule.RuleEngine;
import cn.blmdz.wolf.parana.spu.dto.FullSpu;
import cn.blmdz.wolf.parana.spu.service.SpuReadService;

@Component
public class SpuReader {
   private static final Logger log = LoggerFactory.getLogger(SpuReader.class);
   private final SpuReadService spuReadService;
   private final RuleEngine ruleEngine;

   @Autowired
   public SpuReader(SpuReadService spuReadService, RuleEngine ruleEngine) {
      this.spuReadService = spuReadService;
      this.ruleEngine = ruleEngine;
   }

   @Export(
      paramNames = {"spuId"}
   )
   public Response findForEdit(Long spuId) {
      Response<FullSpu> rFullSpu = this.spuReadService.findFullInfoBySpuId(spuId);
      if(!rFullSpu.isSuccess()) {
         log.error("failed to find spu(id={}), error code:{}", spuId, rFullSpu.getError());
         return Response.fail(rFullSpu.getError());
      } else {
         FullSpu fullSpu = (FullSpu)rFullSpu.getResult();

         try {
            EditSpu editSpu = new EditSpu();
            editSpu.setSpu(fullSpu.getSpu());
            editSpu.setSpuDetail(fullSpu.getSpuDetail());
            this.ruleEngine.handleOutboundData(fullSpu, editSpu);
            return Response.ok(editSpu);
         } catch (Exception var5) {
            log.error("failed to find spu(id={}) for edit , cause:{}", spuId, Throwables.getStackTraceAsString(var5));
            return Response.fail("spu.find.fail");
         }
      }
   }
}
