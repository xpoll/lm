package cn.blmdz.wolf.web.front.trade;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.google.common.collect.Lists;

import cn.blmdz.home.common.exception.JsonResponseException;
import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.home.common.model.ModelHelper;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Splitters;
import cn.blmdz.hunt.common.UserUtil;
import cn.blmdz.wolf.cart.dto.RichCart;
import cn.blmdz.wolf.cart.model.CartItem;
import cn.blmdz.wolf.cart.service.CartReadService;
import cn.blmdz.wolf.cart.service.CartWriteService;
import cn.blmdz.wolf.parana.item.model.Sku;
import cn.blmdz.wolf.parana.item.service.SkuReadService;
import cn.blmdz.wolf.web.front.component.trade.CartReader;

@RestController
@RequestMapping({"/api/carts"})
public class Carts {
   private static final Logger log = LoggerFactory.getLogger(Carts.class);
   private CartReadService cartReadService;
   private CartWriteService cartWriteService;
   private SkuReadService skuReadService;
   private CartReader cartReader;

   @Autowired
   public Carts(CartReadService cartReadService, CartWriteService cartWriteService, SkuReadService skuReadService, CartReader cartReader) {
      this.cartReadService = cartReadService;
      this.cartWriteService = cartWriteService;
      this.skuReadService = skuReadService;
      this.cartReader = cartReader;
   }

   @RequestMapping(
      method = {RequestMethod.GET}
   )
   public List carts() {
      BaseUser user = UserUtil.getCurrentUser();
      if(user != null) {
         Response<List<RichCart>> findRichCart = this.cartReader.findByUser(user);
         if(findRichCart.isSuccess()) {
            return (List)findRichCart.getResult();
         }

         log.error("fail to find cart for login user:{}, cause:{}", user, findRichCart.getError());
      }

      return Collections.emptyList();
   }

   @RequestMapping(
      method = {RequestMethod.POST}
   )
   @ResponseBody
   public Integer changeCart(@RequestParam("skuId") Long skuId, @RequestParam(
   value = "quantity",
   defaultValue = "1",
   required = false
) Integer quantity) {
      Response<Sku> findSku = this.skuReadService.findSkuById(skuId);
      if(!findSku.isSuccess()) {
         log.error("when changing cart, fail to find sku(id={}) for user(id={}), cause:{}", new Object[]{skuId, UserUtil.getUserId(), findSku.getError()});
         throw new JsonResponseException(findSku.getError());
      } else {
         Sku sku = (Sku)findSku.getResult();
         if(!Objects.equals(sku.getStatus(), Integer.valueOf(1))) {
            throw new JsonResponseException("item.not.available");
         } else {
            Response<Integer> tryChange = this.cartWriteService.changeCart(sku, quantity, UserUtil.getUserId());
            if(!tryChange.isSuccess()) {
               log.error("fail to change cart by skuId={}, quantity={}, userId={}, error code:{}", new Object[]{skuId, quantity, UserUtil.getUserId(), tryChange.getError()});
               throw new JsonResponseException(tryChange.getError());
            } else {
               return (Integer)tryChange.getResult();
            }
         }
      }
   }

   @ResponseBody
   @RequestMapping(
      value = {"/count"},
      method = {RequestMethod.GET},
      produces = {"application/json"}
   )
   public Integer count() {
      if(UserUtil.getCurrentUser() == null) {
         return Integer.valueOf(0);
      } else {
         Response<Integer> getCount = this.cartReadService.count(UserUtil.getUserId());
         if(!getCount.isSuccess()) {
            log.error("fail to get cart item quantity count for user:{}, cause:{}", UserUtil.getUserId(), getCount.getError());
            throw new JsonResponseException(getCount.getError());
         } else {
            return (Integer)getCount.getResult();
         }
      }
   }

   @ResponseBody
   @RequestMapping(
      value = {"/batchDelete"},
      method = {RequestMethod.POST},
      produces = {"application/json"}
   )
   public Boolean batchDelete(@RequestParam("skuIds") String skuData) {
      Long userId = UserUtil.getUserId();
      List<String> stringSkuIds = Splitters.COMMA.splitToList(skuData);
      List<Long> skuIds = new ArrayList();

      for(String skuId : stringSkuIds) {
         skuIds.add(Long.valueOf(skuId));
      }

      if(!skuIds.isEmpty()) {
         Response<Boolean> tryDelete = this.cartWriteService.batchDelete(skuIds, userId);
         if(!tryDelete.isSuccess()) {
            log.error("fail to batch delete permanent skuIds={}, userId={},error code={}", new Object[]{skuIds, userId, tryDelete.getError()});
            throw new JsonResponseException(tryDelete.getError());
         }
      }

      return Boolean.TRUE;
   }

   @RequestMapping(
      value = {"/out-date"},
      method = {RequestMethod.GET},
      produces = {"application/json"}
   )
   @ResponseBody
   public List cleanOutDateSku() {
      Response<List<CartItem>> listCart = this.cartReadService.listByUser(UserUtil.getUserId());
      if(!listCart.isSuccess()) {
         log.error("fail to list cart item by user(id={}), cause:{}", UserUtil.getUserId(), listCart.getError());
      }

      List<CartItem> cartItems = (List)listCart.getResult();
      if(cartItems.isEmpty()) {
         return Collections.emptyList();
      } else {
         List<Long> skuIds = ModelHelper.extractFiled(cartItems, "skuId");
         Response<List<Sku>> findSkus = this.skuReadService.findSkusByIds(skuIds);
         if(!findSkus.isSuccess()) {
            log.error("fail to find sku by ids({}), cause:{}", skuIds, findSkus.getError());
            return Collections.emptyList();
         } else {
            List<Sku> skuList = (List)findSkus.getResult();
            List<Sku> available = Lists.newArrayList();

            for(Sku sku : skuList) {
               if(Objects.equals(sku.getStatus(), Integer.valueOf(1))) {
                  available.add(sku);
               }
            }

            if(available.size() != skuIds.size() && !available.isEmpty()) {
               List<Long> availableIds = ModelHelper.extradIds(available);
               skuIds.removeAll(availableIds);
               Response<Boolean> tryDelete = this.cartWriteService.batchDelete(skuIds, UserUtil.getUserId());
               if(!tryDelete.isSuccess()) {
                  log.error("fail to delete cart items by skuIds({}), cause:{}", skuIds, tryDelete.getError());
                  return Collections.emptyList();
               } else {
                  return skuIds;
               }
            } else {
               return Collections.emptyList();
            }
         }
      }
   }
}
