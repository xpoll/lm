package cn.blmdz.wolf.search.impl.shop.impl;

import java.util.List;
import java.util.concurrent.TimeUnit;

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.base.Stopwatch;
import com.google.common.base.Throwables;
import com.google.common.collect.Iterables;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.home.search.api.IndexExecutor;
import cn.blmdz.home.search.api.model.IndexTask;
import cn.blmdz.wolf.parana.search.dto.IndexedShop;
import cn.blmdz.wolf.parana.search.shop.SearchShopProperties;
import cn.blmdz.wolf.parana.search.shop.ShopDumpService;
import cn.blmdz.wolf.parana.shop.model.Shop;
import cn.blmdz.wolf.shop.impl.dao.ShopDao;

@Component
public class ShopDumpServiceImpl
  implements ShopDumpService
{
  private static final Logger log = LoggerFactory.getLogger(ShopDumpServiceImpl.class);

  private static final DateTimeFormatter DATE_TIME_FORMAT = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss");
  private final ShopDao shopDao;
  private final SearchShopProperties searchShopProperties;
  private final IndexExecutor indexExecutor;
  private final IndexedShopFactory indexedShopFactory;
  private final IndexedShopIndexAction indexedShopIndexAction;

  @Autowired
  public ShopDumpServiceImpl(ShopDao shopDao, SearchShopProperties searchShopProperties, IndexExecutor indexExecutor, IndexedShopFactory indexedShopFactory, IndexedShopIndexAction indexedShopIndexAction)
  {
    this.shopDao = shopDao;
    this.searchShopProperties = searchShopProperties;
    this.indexExecutor = indexExecutor;
    this.indexedShopFactory = indexedShopFactory;
    this.indexedShopIndexAction = indexedShopIndexAction;
  }

  public Response<Boolean> fullDump()
  {
    try {
      Integer fullDumpRange = this.searchShopProperties.getFullDumpRange();
      if (fullDumpRange.intValue() <= 0) {
        fullDumpRange = Integer.valueOf(1095);
      }
      String since = DATE_TIME_FORMAT.print(DateTime.now().withTimeAtStartOfDay().minusDays(fullDumpRange.intValue()));

      log.info("shop full dump start");
      Stopwatch watch = Stopwatch.createStarted();
      int allIndexed = doIndex(since);
      watch.stop();
      log.info("shop full dump end, cost:{} ms, dumped {} shops", Long.valueOf(watch.elapsed(TimeUnit.MILLISECONDS)), Integer.valueOf(allIndexed));

      return Response.ok(Boolean.TRUE);
    } catch (Exception e) {
      log.error("fail to full dump shops,cause:{}", Throwables.getStackTraceAsString(e));
    }return Response.fail("shop.full.dump.fail");
  }

  public Response<Boolean> deltaDump(Integer interval)
  {
    try
    {
      String since = DATE_TIME_FORMAT.print(new DateTime().minusMinutes(interval.intValue()));

      log.info("shop delta dump start");
      Stopwatch watch = Stopwatch.createStarted();
      int allIndexed = doIndex(since);
      watch.stop();
      log.info("shop delta dump end,cost:{} ms, dumped {} shops", Long.valueOf(watch.elapsed(TimeUnit.MILLISECONDS)), Integer.valueOf(allIndexed));

      return Response.ok(Boolean.TRUE);
    } catch (Exception e) {
      log.error("fail to delta dump shops with interval:{},cause:{}", interval, Throwables.getStackTraceAsString(e));
    }
    return Response.fail("shop.delta.dump.fail");
  }

  private int doIndex(String since)
  {
    Long lastId = Long.valueOf(this.shopDao.maxId().longValue() + 1L);

    int allIndexed = 0;
    while (true) {
      List<Shop> shops = this.shopDao.listSince(lastId, since, this.searchShopProperties.getBatchSize().intValue());
      if (Iterables.isEmpty(shops)) {
        break;
      }

      for (Shop shop : shops) {
        try {
          if (shop.getStatus().intValue() > 0) {
            IndexedShop indexedShop = this.indexedShopFactory.create(shop, new Object[0]);
            IndexTask indexTask = this.indexedShopIndexAction.indexTask(indexedShop);
            this.indexExecutor.submit(indexTask);
          } else {
            IndexTask indexTask = this.indexedShopIndexAction.deleteTask(shop.getId());
            this.indexExecutor.submit(indexTask);
          }
        } catch (Exception e) {
          log.error("failed to index shop(id={}),cause:{}", shop.getId(), Throwables.getStackTraceAsString(e));
        }
      }

      allIndexed += shops.size();
      log.info("has indexed {} shops,and last handled id is {}", Integer.valueOf(allIndexed), lastId);
      lastId = ((Shop)Iterables.getLast(shops)).getId();

      if (shops.size() < this.searchShopProperties.getBatchSize().intValue()) {
        break;
      }
    }
    return allIndexed;
  }
}