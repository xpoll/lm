package io.terminus.parana.cart.impl.service;

import com.google.common.base.MoreObjects;
import com.google.common.base.Throwables;
import com.google.common.collect.ImmutableMap;
import io.terminus.common.model.Response;
import io.terminus.parana.cart.impl.dao.CartItemDao;
import io.terminus.parana.cart.model.CartItem;
import io.terminus.parana.cart.service.CartReadService;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class CartReadServiceImpl implements CartReadService {
   private static final Logger log = LoggerFactory.getLogger(CartReadServiceImpl.class);
   private final CartItemDao cartItemDao;

   @Autowired
   public CartReadServiceImpl(CartItemDao cartItemDao) {
      this.cartItemDao = cartItemDao;
   }

   public Response count(Long userId) {
      try {
         Integer total = this.cartItemDao.countCartQuantity(userId);
         return Response.ok(MoreObjects.firstNonNull(total, Integer.valueOf(0)));
      } catch (Exception var3) {
         log.error("fail to get cart count by user(Id={}), cause:{}", userId, Throwables.getStackTraceAsString(var3));
         return Response.fail("cart.count.query.fail");
      }
   }

   public Response listByUser(Long buyerId) {
      try {
         List<CartItem> cartItems = this.cartItemDao.list(ImmutableMap.of("buyerId", buyerId));
         return Response.ok(cartItems);
      } catch (Exception var3) {
         log.error("fail to list cart for user(id={}), cause:{}", buyerId, Throwables.getStackTraceAsString(var3));
         return Response.fail("cart.list.fail");
      }
   }
}
