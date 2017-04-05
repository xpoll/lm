package cn.blmdz.aide.pay.channel.wechatpay;

import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.security.KeyManagementException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.UnrecoverableKeyException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManagerFactory;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Throwables;
import com.google.common.io.Resources;

import cn.blmdz.home.common.util.Arguments;

public class HttpsUtil {
	private static final Logger log = LoggerFactory.getLogger(HttpsUtil.class);
	public static final String SUNX509 = "SunX509";
	public static final String JKS = "JKS";
	public static final String PKCS12 = "PKCS12";
	public static final String TLS = "TLS";
	private static final String USER_AGENT_VALUE = "Mozilla/4.0 (compatible; MSIE 6.0; Windows XP)";
	private static final String JKS_CA_FILENAME = "tenpay_cacert.jks";
	private static final String JKS_CA_ALIAS = "tenpay";
	private static final String JKS_CA_PASSWORD = "";
	private static final String CHARSET = "UTF-8";
	private File caFile;
	private File certFile;
	private InputStream caFileInputStream;
	private InputStream certFileInputStream;
	private static int timeOut = 30;
	private int responseCode;
	private InputStream inputStream;

	private HttpsUtil(String caFilePath, String certFilePath) {
		if (Arguments.notEmpty(caFilePath)) {
			this.caFile = new File(caFilePath);
		} else {
			this.caFile = new File(Resources.getResource("rootca.pem").getPath());

			try {
				this.caFileInputStream = Resources.getResource("rootca.pem").openStream();
			} catch (IOException var5) {
				log.error("can not open stream for cafile, cause {}", Throwables.getStackTraceAsString(var5));
			}
		}

		if (Arguments.notEmpty(certFilePath)) {
			this.certFile = new File(certFilePath);
		} else {
			this.certFile = new File(Resources.getResource("apiclient_cert.p12").getPath());

			try {
				this.certFileInputStream = Resources.getResource("apiclient_cert.p12").openStream();
			} catch (IOException var4) {
				log.error("can not open stream for certFile, cause {}", Throwables.getStackTraceAsString(var4));
			}
		}

	}

	public static HttpsUtil build(String caFilePath, String certFilePath) {
		return new HttpsUtil(caFilePath, certFilePath);
	}

	public HttpsUtil callHttps(String xml, String mchId, String refundGateway) throws IOException, CertificateException,
			KeyStoreException, NoSuchAlgorithmException, UnrecoverableKeyException, KeyManagementException {
		String caPath = this.caFile.getParent();
		File jksCAFile = new File(caPath + "/" + JKS_CA_FILENAME);
		if (!jksCAFile.canWrite()) {
			caPath = (new File(this.getClass().getProtectionDomain().getCodeSource().getLocation().getPath()))
					.getParent();
			jksCAFile = new File(caPath + "/" + JKS_CA_FILENAME);
		}

		if (!jksCAFile.isFile()) {
			X509Certificate cert = (X509Certificate) this.getCertificate(this.caFile);
			FileOutputStream out = new FileOutputStream(jksCAFile);
			storeCACert(cert, JKS_CA_ALIAS, JKS_CA_PASSWORD, out);
			out.close();
		}

		FileInputStream trustStream = new FileInputStream(jksCAFile);
		InputStream keyStream = null;
		if (this.certFile.canRead()) {
			keyStream = new FileInputStream(this.certFile);
		} else {
			keyStream = this.certFileInputStream;
		}

		SSLContext sslContext = getSSLContext(trustStream, JKS_CA_PASSWORD, keyStream, mchId);
		keyStream.close();
		trustStream.close();
		byte[] postData = xml.getBytes(CHARSET);
		this.httpsPostMethod(refundGateway, postData, sslContext);
		return this;
	}

	public String getResContent() throws IOException {
		return this.doResponse();
	}

	public Certificate getCertificate(File cafile) throws CertificateException, IOException {
		CertificateFactory cf = CertificateFactory.getInstance("X.509");
		InputStream in;
		if (cafile.canRead()) {
			in = new FileInputStream(cafile);
		} else {
			in = this.caFileInputStream;
		}

		Certificate cert = cf.generateCertificate(in);
		in.close();
		return cert;
	}

	public static void storeCACert(Certificate cert, String alias, String password, OutputStream out)
			throws KeyStoreException, NoSuchAlgorithmException, CertificateException, IOException {
		KeyStore ks = KeyStore.getInstance("JKS");
		ks.load((InputStream) null, (char[]) null);
		ks.setCertificateEntry(alias, cert);
		ks.store(out, str2CharArray(password));
	}

	public static char[] str2CharArray(String str) {
		return null == str ? null : str.toCharArray();
	}

	public static SSLContext getSSLContext(FileInputStream trustFileInputStream, String trustPasswd,
			InputStream keyFileInputStream, String keyPasswd) throws NoSuchAlgorithmException, KeyStoreException,
			CertificateException, IOException, UnrecoverableKeyException, KeyManagementException {
		TrustManagerFactory tmf = TrustManagerFactory.getInstance("SunX509");
		KeyStore trustKeyStore = KeyStore.getInstance("JKS");
		trustKeyStore.load(trustFileInputStream, str2CharArray(trustPasswd));
		tmf.init(trustKeyStore);
		char[] kp = str2CharArray(keyPasswd);
		KeyManagerFactory kmf = KeyManagerFactory.getInstance("SunX509");
		KeyStore ks = KeyStore.getInstance("PKCS12");
		ks.load(keyFileInputStream, kp);
		kmf.init(ks, kp);
		SecureRandom rand = new SecureRandom();
		SSLContext ctx = SSLContext.getInstance("TLS");
		ctx.init(kmf.getKeyManagers(), tmf.getTrustManagers(), rand);
		return ctx;
	}

	public static HttpsURLConnection getHttpsURLConnection(String strUrl) throws IOException {
		URL url = new URL(strUrl);
		HttpsURLConnection httpsURLConnection = (HttpsURLConnection) url.openConnection();
		return httpsURLConnection;
	}

	protected void httpsPostMethod(String url, byte[] postData, SSLContext sslContext) throws IOException {
		SSLSocketFactory sf = sslContext.getSocketFactory();
		HttpsURLConnection conn = getHttpsURLConnection(url);
		conn.setSSLSocketFactory(sf);
		this.doPost(conn, postData);
	}

	protected void setHttpRequest(HttpURLConnection httpConnection) {
		httpConnection.setConnectTimeout(timeOut * 1000);
		httpConnection.setRequestProperty("User-Agent", USER_AGENT_VALUE);
		httpConnection.setUseCaches(false);
		httpConnection.setDoInput(true);
		httpConnection.setDoOutput(true);
	}

	public static void doOutput(OutputStream out, byte[] data, int len) throws IOException {
		int dataLen = data.length;

		for (int off = 0; off < data.length; out.flush()) {
			if (len >= dataLen) {
				out.write(data, off, dataLen);
				off += dataLen;
			} else {
				out.write(data, off, len);
				off += len;
				dataLen -= len;
			}
		}

	}

	public static String inputStreamTOString(InputStream in, String encoding) throws IOException {
		return new String(inputStreamTOByte(in), encoding);
	}

	public static byte[] inputStreamTOByte(InputStream in) throws IOException {
		int bufferSize = 4096;
		ByteArrayOutputStream outStream = new ByteArrayOutputStream();
		byte[] data = new byte[bufferSize];
		int count = -1;

		while ((count = in.read(data, 0, bufferSize)) != -1) {
			outStream.write(data, 0, count);
		}

		byte[] outByte = outStream.toByteArray();
		outStream.close();
		return outByte;
	}

	protected void doPost(HttpURLConnection conn, byte[] postData) throws IOException {
		conn.setRequestMethod("POST");
		this.setHttpRequest(conn);
		conn.setRequestProperty("Content-Type", "application/x-www-form-urlencoded");
		BufferedOutputStream out = new BufferedOutputStream(conn.getOutputStream());
		doOutput(out, postData, 1024);
		out.close();
		this.responseCode = conn.getResponseCode();
		this.inputStream = conn.getInputStream();
	}

	protected void doGet(HttpURLConnection conn) throws IOException {
		conn.setRequestMethod("GET");
		this.setHttpRequest(conn);
		this.responseCode = conn.getResponseCode();
		this.inputStream = conn.getInputStream();
	}

	protected String doResponse() throws IOException {
		if (null == this.inputStream) {
			return JKS_CA_PASSWORD;
		} else {
			String resContent = inputStreamTOString(this.inputStream, CHARSET);
			this.inputStream.close();
			return resContent;
		}
	}

	public int getResponseCode() {
		return this.responseCode;
	}

	public File getCaFile() {
		return this.caFile;
	}

	public void setCaFile(File caFile) {
		this.caFile = caFile;
	}

	public File getCertFile() {
		return this.certFile;
	}

	public void setCertFile(File certFile) {
		this.certFile = certFile;
	}
}
