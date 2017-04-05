package io.terminus.parana.cart.impl.service;

import com.google.common.base.Throwables;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Maps;
import io.terminus.common.exception.ServiceException;
import io.terminus.common.model.BaseUser;
import io.terminus.common.model.Response;
import io.terminus.common.utils.Arguments;
import io.terminus.parana.cart.impl.dao.CartItemDao;
import io.terminus.parana.cart.model.CartItem;
import io.terminus.parana.cart.service.CartWriteService;
import io.terminus.parana.item.model.Sku;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class CartWriteServiceImpl implements CartWriteService {
   private static final Logger log = LoggerFactory.getLogger(CartWriteServiceImpl.class);
   private final CartItemDao cartItemDao;

   @Autowired
   public CartWriteServiceImpl(CartItemDao cartItemDao) {
      this.cartItemDao = cartItemDao;
   }

   public Response changeCart(Sku sku, Integer quantity, Long userId) {
      Response<Integer> result = new Response();

      try {
         result.setResult(this.doChangeCart(sku, quantity, userId));
      } catch (ServiceException var6) {
         result.setError(var6.getMessage());
      } catch (Exception var7) {
         log.error("fail to change permanent cart by sku={}, quantity={}, userId={},cause:{}", new Object[]{sku, quantity, userId, Throwables.getStackTraceAsString(var7)});
         result.setError("cart.change.fail");
      }

      return result;
   }

   public Response changeCart(Map skuAndQuantities, Long userId) {
      try {
         if(skuAndQuantities != null && !skuAndQuantities.isEmpty()) {
            Map<Long, Integer> finalSkuAndQuantity = Maps.newHashMap();

            for(Entry<Sku, Integer> skuAndQuantity : skuAndQuantities.entrySet()) {
               try {
                  finalSkuAndQuantity.put(((Sku)skuAndQuantity.getKey()).getId(), this.doChangeCart((Sku)skuAndQuantity.getKey(), (Integer)skuAndQuantity.getValue(), userId));
               } catch (ServiceException var7) {
                  log.warn("fail to add sku:{} quantity:{} to cart for user:{}, cause:{}", new Object[]{skuAndQuantity.getKey(), skuAndQuantity.getValue(), userId, var7.getMessage()});
               }
            }

            return Response.ok(finalSkuAndQuantity);
         } else {
            return Response.ok(Collections.emptyMap());
         }
      } catch (Exception var8) {
         log.error("fail batch add {} to cart for user:{}, cause:{}", new Object[]{skuAndQuantities, userId, Throwables.getStackTraceAsString(var8)});
         return Response.fail("cart.change.fail");
      }
   }

   public Response deleteById(Long id) {
      try {
         this.cartItemDao.delete(id);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var3) {
         log.error("fail to delete CartItem(id={}), cause:{}", id, Throwables.getStackTraceAsString(var3));
         return Response.fail("cart.delete.fail");
      }
   }

   public Response deleteCart(Long userId) {
      try {
         CartItem cartItem = new CartItem();
         cartItem.setBuyerId(userId);
         this.cartItemDao.deleteBy(cartItem);
         return Response.ok(Boolean.TRUE);
      } catch (Exception var3) {
         log.error("fail to delete cart by userIdOrCookie={},cause:{}", userId, Throwables.getStackTraceAsString(var3));
         return Response.fail("cart.delete.fail");
      }
   }

   public Response batchDelete(List skuIds, Long userId) {
      try {
         if(Arguments.isNullOrEmpty(skuIds)) {
            return Response.ok(Boolean.TRUE);
         } else {
            this.cartItemDao.batchDeleteBySkuIds(userId, skuIds);
            return Response.ok(Boolean.TRUE);
         }
      } catch (Exception var4) {
         log.error("fail to batch delete permanent by skuIds={},userId={},cause:{}", new Object[]{skuIds, userId, Throwables.getStackTraceAsString(var4)});
         return Response.fail("cart.delete.fail");
      }
   }

   public Response submitCart(Map skuAndQuantity, BaseUser buyer) {
      return null;
   }

   private Integer doChangeCart(Sku sku, Integer quantity, Long userId) {
      List<CartItem> cartItems = this.cartItemDao.list(ImmutableMap.of("buyerId", userId, "skuId", sku.getId()));
      if(cartItems.isEmpty() && quantity.intValue() <= 0) {
         return Integer.valueOf(0);
      } else if(cartItems.isEmpty()) {
         CartItem cartItem = new CartItem();
         cartItem.setBuyerId(userId);
         cartItem.setQuantity(quantity);
         cartItem.setShopId(sku.getShopId());
         cartItem.setSkuId(sku.getId());
         cartItem.setSnapshotPrice(sku.getPrice());
         this.cartItemDao.create(cartItem);
         return quantity;
      } else {
         CartItem cartItem = (CartItem)cartItems.get(0);
         Integer current = Integer.valueOf(cartItem.getQuantity().intValue() + quantity.intValue());
         if(current.intValue() <= 0) {
            this.cartItemDao.delete(cartItem.getId());
            return Integer.valueOf(0);
         } else {
            cartItem.setSnapshotPrice(sku.getPrice());
            cartItem.setQuantity(current);
            this.cartItemDao.update(cartItem);
            return current;
         }
      }
   }
}
