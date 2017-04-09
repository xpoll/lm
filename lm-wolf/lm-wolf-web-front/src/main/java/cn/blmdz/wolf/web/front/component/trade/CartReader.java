package cn.blmdz.wolf.web.front.component.trade;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.base.Function;
import com.google.common.base.Throwables;
import com.google.common.collect.FluentIterable;
import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.Lists;

import cn.blmdz.home.common.exception.ServiceException;
import cn.blmdz.home.common.model.BaseUser;
import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.common.util.Arguments;
import cn.blmdz.hunt.protocol.Export;
import cn.blmdz.wolf.cache.ItemCacher;
import cn.blmdz.wolf.cart.dto.RichCart;
import cn.blmdz.wolf.cart.dto.RichCartItem;
import cn.blmdz.wolf.cart.model.CartItem;
import cn.blmdz.wolf.cart.service.CartReadService;
import cn.blmdz.wolf.cart.service.CartWriteService;
import cn.blmdz.wolf.item.model.Item;
import cn.blmdz.wolf.item.model.Sku;
import cn.blmdz.wolf.item.service.SkuReadService;
import cn.blmdz.wolf.shop.model.Shop;
import cn.blmdz.wolf.shop.service.ShopReadService;

@Component
public class CartReader {
   private static final Logger log = LoggerFactory.getLogger(CartReader.class);
   private CartReadService cartReadService;
   private CartWriteService cartWriteService;
   private SkuReadService skuReadService;
   private ItemCacher itemCacher;
   private ShopReadService shopReadService;

   @Autowired
   public CartReader(CartReadService cartReadService, CartWriteService cartWriteService, SkuReadService skuReadService, ItemCacher itemCacher, ShopReadService shopReadService) {
      this.cartReadService = cartReadService;
      this.cartWriteService = cartWriteService;
      this.skuReadService = skuReadService;
      this.itemCacher = itemCacher;
      this.shopReadService = shopReadService;
   }

   @Export(
      paramNames = {"baseUser"}
   )
   public Response findByUser(BaseUser baseUser) {
      Response<List<RichCart>> result = new Response();

      try {
         List<RichCart> richCarts = this.getUserCart(baseUser.getId());
         result.setResult(richCarts);
         return result;
      } catch (Exception var4) {
         log.error("fail to get permanent cart by userId={}, cause:{}", baseUser.getId(), Throwables.getStackTraceAsString(var4));
         result.setError("cart.query.fail");
         return result;
      }
   }

   protected List getUserCart(Long userId) {
      Response<List<CartItem>> findCartItems = this.cartReadService.listByUser(userId);
      if(!findCartItems.isSuccess()) {
         log.error("find cart items for user(id={}) failed, cause:{}", userId, findCartItems.getError());
         throw new ServiceException(findCartItems.getError());
      } else {
         List<CartItem> cartItems = (List)findCartItems.getResult();
         if(Arguments.isNullOrEmpty(cartItems)) {
            return Collections.emptyList();
         } else {
            ImmutableListMultimap<Long, CartItem> shopIdCartItems = FluentIterable.from(cartItems).index(new Function<CartItem, Long>() {
               public Long apply(CartItem input) {
                  return input.getShopId();
               }
            });
            List<RichCart> richCarts = new ArrayList();

            for(Long shopId : shopIdCartItems.keySet()) {
               Response<Shop> findShop = this.shopReadService.findById(shopId);
               if(findShop.isSuccess() && findShop.getResult() != null) {
                  Shop shop = (Shop)findShop.getResult();
                  List<RichCartItem> richCartItems = this.convertToRichCartItem(shopIdCartItems.get(shopId));
                  if(!richCartItems.isEmpty()) {
                     RichCart richCart = new RichCart();
                     richCart.setShopId(shopId);
                     richCart.setShopId(shopId);
                     richCart.setShopName(shop.getName());
                     richCart.setShopImage(shop.getImageUrl());
                     richCart.setSellerId(shop.getUserId());
                     richCart.setCartItems(richCartItems);
                     richCart.flushUpdateAt();
                     richCarts.add(richCart);
                  }
               } else {
                  log.warn("fail to find shop by id={} when view cart by {}, error code:{}, skip", new Object[]{shopId, userId, findShop.getError()});
               }
            }

            Collections.sort(richCarts);
            return richCarts;
         }
      }
   }

   protected List convertToRichCartItem(List<CartItem> cartItems) {
      List<RichCartItem> richCartItems = Lists.newArrayList();

      for(CartItem cartItem : cartItems) {
         Response<Sku> findSku = this.skuReadService.findSkuById(cartItem.getSkuId());
         if(findSku.isSuccess() && findSku.getResult() != null) {
            Sku sku = (Sku)findSku.getResult();
            Item item = this.itemCacher.findItemById(sku.getItemId());
            if(Objects.equals(item.getStatus(), Integer.valueOf(-3))) {
               log.warn("item id={} already deleted, remove from cart", item.getId());
               this.cartWriteService.deleteById(cartItem.getId());
            } else {
               RichCartItem richCartItem = new RichCartItem();
               richCartItem.setSku(sku);
               richCartItem.setCartItem(cartItem);
               richCartItem.setItemName(item.getName());
               richCartItem.setItemStatus(item.getStatus());
               richCartItem.setItemImage(item.getMainImage());
               richCartItems.add(richCartItem);
            }
         } else {
            log.warn("fail to find sku by id={}, error code={}, skip", cartItem.getSkuId(), findSku.getError());
         }
      }

      return richCartItems;
   }
}
