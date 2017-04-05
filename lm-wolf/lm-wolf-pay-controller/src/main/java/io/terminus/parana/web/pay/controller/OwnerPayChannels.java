package io.terminus.parana.web.pay.controller;

import io.terminus.common.exception.JsonResponseException;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Arguments;
import io.terminus.pampas.design.model.Site;
import io.terminus.pampas.design.service.SiteService;
import io.terminus.pampas.engine.utils.Domains;
import io.terminus.parana.pay.enums.OwnerType;
import io.terminus.parana.pay.model.OwnerPayChannel;
import io.terminus.parana.pay.service.PayReadService;
import io.terminus.parana.pay.service.PayWriteService;
import javax.servlet.http.HttpServletRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

public abstract class OwnerPayChannels {
   private static final Logger log = LoggerFactory.getLogger(OwnerPayChannels.class);
   protected final PayReadService payReadService;
   protected final SiteService siteService;
   protected final PayWriteService payWriteService;

   public OwnerPayChannels(PayReadService payReadService, SiteService siteService, PayWriteService payWriteService) {
      this.payReadService = payReadService;
      this.siteService = siteService;
      this.payWriteService = payWriteService;
   }

   public abstract OwnerPayChannel getOwnerPayChannel(String var1, Integer var2, HttpServletRequest var3);

   protected OwnerPayChannel getOwnerPayChannel(Long shopOwnerId, HttpServletRequest request) {
      if(shopOwnerId != null) {
         Response<OwnerPayChannel> opcRes = this.payReadService.findOwnerPayChannelByOwnerIdAndType(shopOwnerId, Integer.valueOf(OwnerType.SHOP.value()));
         if(!opcRes.isSuccess()) {
            throw new JsonResponseException(opcRes.getError());
         }

         if(Arguments.notNull(opcRes.getResult())) {
            return (OwnerPayChannel)opcRes.getResult();
         }
      }

      String domain = Domains.getDomainFromRequest(request);
      Site site = this.siteService.findByDomain(domain);
      if(Arguments.isNull(site)) {
         throw new JsonResponseException("站点不存在");
      } else {
         Response<OwnerPayChannel> opcRes = this.payReadService.findOwnerPayChannelByOwnerIdAndType(site.getId(), Integer.valueOf(OwnerType.SITE.value()));
         if(!opcRes.isSuccess()) {
            throw new JsonResponseException(opcRes.getError());
         } else {
            return (OwnerPayChannel)opcRes.getResult();
         }
      }
   }

   @RequestMapping(
      value = {"/pay_channel"},
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   @ResponseBody
   public Boolean createOwnerPayChannel(@RequestBody OwnerPayChannel ownerPayChannel) {
      Response<Boolean> response = this.payWriteService.createOwnerPayChannel(ownerPayChannel);
      if(!response.isSuccess()) {
         throw new JsonResponseException(response.getError());
      } else {
         return (Boolean)response.getResult();
      }
   }

   @RequestMapping(
      value = {"/pay_channel"},
      method = {RequestMethod.PUT},
      produces = {"application/json"}
   )
   @ResponseBody
   public Boolean updateOwnerPayChannel(@RequestBody OwnerPayChannel ownerPayChannel) {
      Response<Boolean> response = this.payWriteService.updateOwnerPayChannel(ownerPayChannel);
      if(!response.isSuccess()) {
         throw new JsonResponseException(response.getError());
      } else {
         return (Boolean)response.getResult();
      }
   }
}
