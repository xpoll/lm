package cn.blmdz.wolf.category.impl.service;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Queue;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.base.Throwables;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Maps;
import com.google.common.collect.Multimap;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.category.dto.ShopCategoryWithChildren;
import cn.blmdz.wolf.category.impl.dao.ShopCategoryDao;
import cn.blmdz.wolf.category.model.ShopCategory;
import cn.blmdz.wolf.category.service.ShopCategoryReadService;

@Service
public class ShopCategoryReadServiceImpl implements ShopCategoryReadService {
   private static final Logger log = LoggerFactory.getLogger(ShopCategoryReadServiceImpl.class);
   private final ShopCategoryDao shopCategoryDao;

   @Autowired
   public ShopCategoryReadServiceImpl(ShopCategoryDao shopCategoryDao) {
      this.shopCategoryDao = shopCategoryDao;
   }

   public Response findById(Long id) {
      try {
         ShopCategory shopCategory = (ShopCategory)this.shopCategoryDao.findById(id);
         if(shopCategory == null) {
            log.error("no shop category(categoryId={}) found", id);
            return Response.fail("shopCategory.not.found");
         } else {
            return Response.ok(shopCategory);
         }
      } catch (Exception var3) {
         log.error("failed to find shop category(id={}), cause:{}", id, Throwables.getStackTraceAsString(var3));
         return Response.fail("category.find.fail");
      }
   }

   public Response findChildrenByShopId(Long shopId) {
      try {
         List<ShopCategory> result = this.shopCategoryDao.findChildrenByShopIdAndPid(shopId, Long.valueOf(0L));
         if(!result.isEmpty()) {
            Collections.sort(result);
         }

         return Response.ok(result);
      } catch (Exception var3) {
         log.error("failed to find children of shop category(shopId={}, categoryId={}), cause:{}", shopId, Throwables.getStackTraceAsString(var3));
         return Response.fail("category.find.fail");
      }
   }

   public Response findChildrenByShopIdAndPid(Long shopId, Long pid) {
      try {
         List<ShopCategory> result = this.shopCategoryDao.findChildrenByShopIdAndPid(shopId, pid);
         if(!result.isEmpty()) {
            Collections.sort(result);
         }

         return Response.ok(result);
      } catch (Exception var4) {
         log.error("failed to find children of shop category(shopId={}, categoryId={}), cause:{}", new Object[]{shopId, pid, Throwables.getStackTraceAsString(var4)});
         return Response.fail("category.find.fail");
      }
   }

   public Response findEntireTreeByShopId(Long shopId) {
      try {
         List<ShopCategory> db = this.shopCategoryDao.findByShopId(shopId);
         if(!db.isEmpty()) {
            Collections.sort(db);
         }

         List<ShopCategoryWithChildren> result = this.buildTree(db);
         log.debug("build tree success, count-for-first-level={}", Integer.valueOf(result.size()));
         return Response.ok(result);
      } catch (Exception var4) {
         log.error("find all tree by shopId={} failed, cause:{}", shopId, Throwables.getStackTraceAsString(var4));
         return Response.fail("category.find.fail");
      }
   }

   private List buildTree(List<ShopCategory> db) {
      if(db != null && !db.isEmpty()) {
         Map<Long, ShopCategory> idMap = Maps.newHashMap();
         Multimap<Long, Long> pidMap = ArrayListMultimap.create();

         for(ShopCategory shopCategory : db) {
            Long id = shopCategory.getId();
            Long pid = shopCategory.getPid();
            if(id == null) {
               log.warn("shop category id null, object={}", shopCategory);
            } else {
               idMap.put(id, shopCategory);
               if(pid == null) {
                  log.warn("shop category pid null, id={}", id);
               } else {
                  pidMap.put(pid, id);
               }
            }
         }

         Map<Long, ShopCategoryWithChildren> resultMap = Maps.newHashMap();
         resultMap.put(Long.valueOf(0L), this.buildRoot());
         Queue<Long> q = new ArrayDeque();
         q.addAll(pidMap.get(Long.valueOf(0L)));

         while(!((Queue)q).isEmpty()) {
            Long id = (Long)q.poll();
            ShopCategory sc = (ShopCategory)idMap.get(id);
            Long pid = sc.getPid();
            ShopCategoryWithChildren cur = this.buildNode(sc);
            ShopCategoryWithChildren parent = (ShopCategoryWithChildren)resultMap.get(pid);
            parent.getChildren().add(cur);
            resultMap.put(id, cur);
            q.addAll(pidMap.get(id));
         }

         return ((ShopCategoryWithChildren)resultMap.get(Long.valueOf(0L))).getChildren();
      } else {
         return Collections.emptyList();
      }
   }

   private ShopCategoryWithChildren buildRoot() {
      ShopCategoryWithChildren root = new ShopCategoryWithChildren();
      root.setChildren(new ArrayList());
      return root;
   }

   private ShopCategoryWithChildren buildNode(ShopCategory sc) {
      ShopCategoryWithChildren r = new ShopCategoryWithChildren();
      BeanUtils.copyProperties(sc, r);
      r.setChildren(new ArrayList());
      return r;
   }
}
