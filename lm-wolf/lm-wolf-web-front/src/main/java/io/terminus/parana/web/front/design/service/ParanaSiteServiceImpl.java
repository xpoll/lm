package io.terminus.parana.web.front.design.service;

import com.google.common.base.Strings;
import io.terminus.common.redis.utils.JedisTemplate;
import io.terminus.common.redis.utils.JedisTemplate.JedisAction;
import io.terminus.pampas.design.dao.PageDao;
import io.terminus.pampas.design.dao.SiteDao;
import io.terminus.pampas.design.model.Page;
import io.terminus.pampas.design.model.Site;
import io.terminus.pampas.design.model.Page.Type;
import io.terminus.pampas.engine.config.model.Render.Layout;
import io.terminus.pampas.engine.service.ConfigService;
import io.terminus.parana.web.front.design.service.ParanaSiteService;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.stereotype.Service;
import redis.clients.jedis.Jedis;
import redis.clients.jedis.Transaction;

@ConditionalOnBean({ConfigService.class})
@Service
public class ParanaSiteServiceImpl implements ParanaSiteService {
   @Autowired
   private ConfigService configService;
   @Autowired
   private SiteDao siteDao;
   @Autowired
   private PageDao pageDao;
   @Autowired
   @Qualifier("pampasJedisTemplate")
   private JedisTemplate jedisTemplate;

   public Long getCurrentShopSiteIdByApp(final String appKey) {
      String siteIdStr = (String)this.jedisTemplate.execute(new JedisAction() {
         public String action(Jedis jedis) {
            return jedis.get(ParanaSiteServiceImpl.this.keyForActiveShopSite(appKey));
         }
      });
      return Strings.isNullOrEmpty(siteIdStr)?this.initSite(appKey):Long.valueOf(siteIdStr);
   }

   private Long initSite(final String appKey) {
      return (Long)this.jedisTemplate.execute(new JedisAction() {
         public Long action(Jedis jedis) {
            String idStr = jedis.get(ParanaSiteServiceImpl.this.keyForActiveShopSite(appKey));
            if(!Strings.isNullOrEmpty(idStr)) {
               return Long.valueOf(idStr);
            } else {
               List<Layout> layouts = ParanaSiteServiceImpl.this.configService.listLayouts(appKey, "SHOP");
               Layout layout = layouts.isEmpty()?null:(Layout)layouts.get(0);
               if(layout == null) {
                  throw new IllegalStateException("no default layout found for app " + appKey);
               } else {
                  Transaction t = jedis.multi();
                  Long siteId = ParanaSiteServiceImpl.this.createSite(layout.getApp(), layout, t);
                  t.set(ParanaSiteServiceImpl.this.keyForActiveShopSite(appKey), siteId.toString());
                  t.exec();
                  return siteId;
               }
            }
         }
      });
   }

   private Long createSite(String app, Layout layout, Transaction t) {
      Site site = new Site();
      site.setApp(app);
      site.setLayout(layout.getKey());
      site.setLayoutName(layout.getName());
      site.setUserId(Long.valueOf(1L));
      site.setId(this.siteDao.newId());
      this.siteDao.create(t, site, true);
      t.set(this.keyForShopSite(app, layout.getKey()), site.getId().toString());
      String pageName = "商品详情";
      Page page = new Page();
      page.setImmutable(true);
      page.setApp(site.getApp());
      page.setSiteId(site.getId());
      page.setName(pageName);
      page.setTitle(pageName);
      page.setPath("item");
      page.setType(Type.PAGE);
      this.pageDao.create(page, t);
      return site.getId();
   }

   private String keyForActiveShopSite(String app) {
      return "app:" + app + ":active-shop-site";
   }

   private String keyForShopSite(String app, String layoutKey) {
      return "app:" + app + ":layout:" + layoutKey + ":site";
   }
}
