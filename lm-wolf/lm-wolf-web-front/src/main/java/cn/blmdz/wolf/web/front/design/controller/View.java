package cn.blmdz.wolf.web.front.design.controller;

import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.google.common.base.Supplier;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.hunt.common.UserUtil;
import cn.blmdz.hunt.design.container.DPageRender;
import cn.blmdz.hunt.design.medol.Site;
import cn.blmdz.hunt.design.service.SiteService;
import cn.blmdz.hunt.engine.exception.NotFound404Exception;
import cn.blmdz.hunt.engine.request.ViewRender;
import cn.blmdz.hunt.engine.utils.Domains;
import cn.blmdz.wolf.item.model.Item;
import cn.blmdz.wolf.item.service.ItemReadService;
import cn.blmdz.wolf.web.front.design.service.ParanaSiteService;

public class View {
   private static final Logger log = LoggerFactory.getLogger(View.class);
   @Autowired
   private DPageRender dPageRender;
   @Autowired
   private ViewRender viewRender;
   @Autowired
   private SiteService siteService;
   @Autowired
   private ParanaSiteService paranaSiteService;
   @Autowired
   private ItemReadService itemReadService;

   @RequestMapping(
      value = {"/items/{itemId}"},
      method = {RequestMethod.GET}
   )
   public void item(HttpServletRequest request, HttpServletResponse response, @PathVariable Long itemId, @RequestParam Map context) {
      Response<Item> itemR = this.itemReadService.findById(itemId);
      if(!itemR.isSuccess()) {
         log.error("failed to find item(id={}),error code:{}", itemId, itemR.getError());
         throw new NotFound404Exception(itemR.getError());
      } else {
         Item item = (Item)itemR.getResult();
         context.put("itemId", itemId);
         context.put("spuId", item.getSpuId());
         context.put("buyerId", UserUtil.getUserId());
         context.put("shopId", item.getShopId());
         this.renderItemDetail(request, response, item.getShopId(), context);
      }
   }

   private void renderItemDetail(HttpServletRequest request, HttpServletResponse response, Long sellerId, final Map context) {
      final Long siteId = this.paranaSiteService.getCurrentShopSiteIdByApp("PC");
      context.put("sellerId", sellerId);
      String domain = Domains.getDomainFromRequest(request);
      Site mainSite = this.siteService.findByDomain(domain);
      if(mainSite != null) {
         context.put("_MAIN_SITE_", mainSite);
      }

      this.viewRender.render(request, response, new Supplier() {
         public String get() {
            return View.this.dPageRender.render(siteId, "item", context);
         }
      });
   }
}
