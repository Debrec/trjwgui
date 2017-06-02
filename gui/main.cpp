#include <windows.h>
#include <string>
#include <iostream>
#include <cstdlib>
#include <fstream>
//#include <pthread.h>

using namespace std;

#define ID_LABEL1 501
#define ID_TEXT1 502
#define ID_LABEL2 503
#define ID_TEXT2 504
#define ID_LABEL3 505
#define ID_TEXT3 506
#define ID_LABEL4 507
#define ID_TEXT4 508
#define ID_LABEL5 509
#define ID_TEXT5 510
#define ID_LABEL6 511
#define ID_TEXT6 512
#define ID_LABEL7 513
#define ID_TEXT7 514
#define ID_LABEL8 515
#define ID_TEXT8 516
#define ID_LABEL9 517
#define ID_TEXT9 518
#define ID_LABEL10 519
#define ID_TEXT10 520
#define ID_LABEL11 521
#define ID_TEXT11 522
#define ID_LABEL12 523
#define ID_TEXT12 524
#define ID_LABEL13 525
#define ID_TEXT13 526
#define ID_LABEL14 527
#define ID_TEXT14 528
#define ID_LABEL15 529
#define ID_TEXT15 530
#define ID_LABEL16 531
#define ID_TEXT16 532
#define ID_LABEL17 533
#define ID_TEXT17 534

#define ID_BUTTON_GUARDAR 601
#define ID_BUTTON_CANCELAR 602
#define ID_BUTTON_RESET 603
#define ID_BUTTON_RUN 604

extern "C" void trj_();

using namespace std;

class datos {
public:
    int nTime, nOut, nPartX, nPartY;
    float mLon, iLon, mLat, iLat;
    string windFile, tracFile;
    int isec, day, mon, year;
    float theta;
    int its;
    int gFile;
	string File;
};

HFONT hfont;

LRESULT CALLBACK WindowProcedure(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam);

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
   LPSTR lpszCmdParam, int nCmdShow)
{
   /* Declaraci�n: */
   HWND hwnd;
   MSG mensaje;
   WNDCLASSEX wincl;

   /* Inicializaci�n: */
   /* Estructura de la ventana */
   wincl.hInstance = hInstance;
   wincl.lpszClassName = "TRJCFG";
   wincl.lpfnWndProc = WindowProcedure;
   wincl.style = CS_DBLCLKS;
   wincl.cbSize = sizeof(WNDCLASSEX);

   /* Usar icono y puntero por defecto */
   wincl.hIcon = LoadIcon(NULL, IDI_APPLICATION);
   wincl.hIconSm = LoadIcon(NULL, IDI_APPLICATION);
   wincl.hCursor = LoadCursor(NULL, IDC_ARROW);
   wincl.lpszMenuName = NULL;
   wincl.cbClsExtra = 0;
   wincl.cbWndExtra = 0;
   wincl.hbrBackground = GetSysColorBrush(COLOR_WINDOW);
;

   /* Registrar la clase de ventana, si falla, salir del programa */
   if(!RegisterClassEx(&wincl)) return 0;

   hwnd = CreateWindowEx(
           0,
           "TRJCFG",
           "TRJCFG",
           WS_OVERLAPPEDWINDOW,
           CW_USEDEFAULT,
           CW_USEDEFAULT,
           700,
           375,
           HWND_DESKTOP,
           NULL,
           hInstance,
           NULL
   );

   ShowWindow(hwnd, SW_SHOWDEFAULT);

   /* Bucle de mensajes: */
   while(TRUE == GetMessage(&mensaje, 0, 0, 0))
   {
      TranslateMessage(&mensaje);
      DispatchMessage(&mensaje);
   }

   return mensaje.wParam;
}


int CrearControl(HWND hwnd,LPARAM lParam,string label,int x,int y,int w,int h,long ID_TEXTO,int wst=60) {
    HINSTANCE hInstance = ((LPCREATESTRUCT)lParam)->hInstance;


    HWND shctrl = CreateWindowEx(
        0,
        "STATIC",          /* Nombre de la clase */
        label.c_str(),
        ES_LEFT | WS_CHILD | WS_VISIBLE | WS_TABSTOP, /* Estilo */
        x, y,          /* Posici�n */
        wst, h,         /* Tama�o */
        hwnd,            /* Ventana padre */
        (HMENU)ID_TEXTO, /* Identificador del control */
        hInstance,       /* Instancia */
        NULL);           /* Sin datos de creaci�n de ventana */
        /* Inicializaci�n de los datos de la aplicaci�n */
    SetDlgItemText(hwnd, ID_TEXTO, label.c_str());
		//hfont = (HFONT)GetStockObject( DEFAULT_GUI_FONT );
//SendMessage(shctrl, WM_SETFONT, (WPARAM)hfont, MAKELPARAM(TRUE, 0));
    /* Insertar control Edit */
    HWND hctrl = CreateWindowEx(
        0,
        "EDIT",          /* Nombre de la clase */
        "",              /* Texto del t�tulo, no tiene */
        ES_LEFT | WS_CHILD | WS_VISIBLE | WS_BORDER | WS_TABSTOP, /* Estilo */
        x+wst+10, y,          /* Posici�n */
        w, h,         /* Tama�o */
        hwnd,            /* Ventana padre */
        (HMENU)(ID_TEXTO+1), /* Identificador del control */
        hInstance,       /* Instancia */
        NULL);           /* Sin datos de creaci�n de ventana */
        /* Inicializaci�n de los datos de la aplicaci�n */
    /*hfont = CreateFont(20, 0, 0, 0, 300,
              FALSE, FALSE, FALSE, DEFAULT_CHARSET,
              OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS,
              PROOF_QUALITY, DEFAULT_PITCH | FF_ROMAN,
              "Times New Roman");*/

    SendMessage(hctrl, WM_SETFONT, (WPARAM)hfont, MAKELPARAM(TRUE, 0));

    //SetDlgItemText(hwnd, ID_TEXTO+1, edit.c_str());
    //SetFocus(hctrl);
    return 0;
}

int CrearBoton(HWND hwnd,LPARAM lParam,int x,int y,int w,int h,long ID_BOTON,string texto) {

    HINSTANCE hInstance = ((LPCREATESTRUCT)lParam)->hInstance;
    CreateWindowEx(
      0,
      "BUTTON",        /* Nombre de la clase */
      texto.c_str(),       /* Texto del t�tulo */
      BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE | WS_BORDER | WS_TABSTOP, /* Estilo */
      x, y,           /* Posici�n */
      w, h,          /* Tama�o */
      hwnd,            /* Ventana padre */
      (HMENU)ID_BOTON, /* Identificador del control */
      hInstance,       /* Instancia */
      NULL);           /* Sin datos de creaci�n de ventana */
      return 0;
}

int LeerArchivo(string nombreArchivo,datos &data) {
    ifstream archivo(nombreArchivo.c_str());
		if (!archivo.fail()) {
    	archivo>>data.nTime>>data.nOut;
    	archivo>>data.nPartX>>data.nPartY;
    	archivo>>data.mLon>>data.iLon>>data.mLat>>data.iLat;
    	char buffer1[500],buffer2[500];
    	archivo>>buffer1>>buffer2;
    	data.windFile.assign(buffer1);
    	data.tracFile.assign(buffer2);
    	archivo>>data.isec>>data.day>>data.mon>>data.year;
    	archivo>>data.theta;
    	archivo>>data.its;
    	archivo>>data.gFile;
    	archivo.close();
		} else {
			return 1;
		}
    return 0;
}

int GuardarArchivo(string nombreArchivo,datos &data) {
    ofstream archivo(nombreArchivo.c_str());
    archivo<<data.nTime<<" "<<data.nOut<<endl;
    archivo<<data.nPartX<<" "<<data.nPartY<<endl;
    archivo<<data.mLon<<" "<<data.iLon<<" "<<data.mLat<<" "<<data.iLat<<endl;
    archivo<<data.windFile.c_str()<<" "<<data.tracFile.c_str()<<endl;
    archivo<<data.isec<<" "<<data.day<<" "<<data.mon<<" "<<data.year<<endl;
    archivo<<data.theta<<endl;
    archivo<<data.its<<endl;
    archivo<<data.gFile<<endl;
    return 0;
}

//se cargan los datos de la clase data en el formulario
int Resetear(HWND hwnd,datos data) {
    char buffer[500];
    //itoa(data.nTime,buffer,10);
		sprintf(buffer,"%d",data.nTime);
		SetDlgItemText(hwnd, ID_TEXT1,buffer);
		sprintf(buffer,"%d",data.nOut);
//    itoa(data.nOut,buffer,10);
    SetDlgItemText(hwnd, ID_TEXT2,buffer);

		sprintf(buffer,"%d",data.nPartX);
//    itoa(data.nPartX,buffer,10);
    SetDlgItemText(hwnd, ID_TEXT3,buffer);
//    itoa(data.nPartY,buffer,10);
		sprintf(buffer,"%d",data.nPartY);
    SetDlgItemText(hwnd, ID_TEXT4,buffer);

    sprintf(buffer,"%.2f",data.mLon);
    SetDlgItemText(hwnd, ID_TEXT5,buffer);
    sprintf(buffer,"%.2f",data.iLon);
    SetDlgItemText(hwnd, ID_TEXT6,buffer);
    sprintf(buffer,"%.2f",data.mLat);
    SetDlgItemText(hwnd, ID_TEXT7,buffer);
    sprintf(buffer,"%.2f",data.iLat);
    SetDlgItemText(hwnd, ID_TEXT8,buffer);

    SetDlgItemText(hwnd, ID_TEXT9,data.windFile.c_str());
    SetDlgItemText(hwnd, ID_TEXT10,data.tracFile.c_str());

    sprintf(buffer,"%d",data.isec);
    SetDlgItemText(hwnd, ID_TEXT11,buffer);
    sprintf(buffer,"%d",data.day);
    SetDlgItemText(hwnd, ID_TEXT12,buffer);
    sprintf(buffer,"%d",data.mon);
    SetDlgItemText(hwnd, ID_TEXT13,buffer);
    sprintf(buffer,"%d",data.year);
    SetDlgItemText(hwnd, ID_TEXT14,buffer);

    sprintf(buffer,"%.2f",data.theta);
    SetDlgItemText(hwnd, ID_TEXT15,buffer);

		sprintf(buffer,"%d",data.its);
//    itoa(data.its,buffer,10);
    SetDlgItemText(hwnd, ID_TEXT16,buffer);

		sprintf(buffer,"%d",data.gFile);
//    itoa(data.gFile,buffer,10);
    SetDlgItemText(hwnd, ID_TEXT17,buffer);
    return 0;
}

int CrearControles(HWND hwnd,LPARAM lParam) {
    CrearControl(hwnd,lParam,"Ntime",36,20,60,20,ID_LABEL1);
    CrearControl(hwnd,lParam,"Nout",180,20,60,20,ID_LABEL2);

    CrearControl(hwnd,lParam,"Npartx",36,50,60,20,ID_LABEL3);
    CrearControl(hwnd,lParam,"Nparty",180,50,60,20,ID_LABEL4);

    CrearControl(hwnd,lParam,"mLon",36,80,80,20,ID_LABEL5);
    CrearControl(hwnd,lParam,"iLon",196,80,60,20,ID_LABEL6);
    CrearControl(hwnd,lParam,"mLat",336,80,60,20,ID_LABEL7);
    CrearControl(hwnd,lParam,"iLat",476,80,60,20,ID_LABEL8);

    CrearControl(hwnd,lParam,"windFile",36,110,250,20,ID_LABEL9);
    CrearControl(hwnd,lParam,"tracFile",36,140,250,20,ID_LABEL10);

    CrearControl(hwnd,lParam,"isec",36,170,60,20,ID_LABEL11);
    CrearControl(hwnd,lParam,"day",180,170,60,20,ID_LABEL12);
    CrearControl(hwnd,lParam,"mon",324,170,60,20,ID_LABEL13);
    CrearControl(hwnd,lParam,"year",468,170,60,20,ID_LABEL14);

    CrearControl(hwnd,lParam,"theta",36,200,80,20,ID_LABEL15);

    CrearControl(hwnd,lParam,"its",36,230,60,20,ID_LABEL16);

    CrearControl(hwnd,lParam,"GF",36,260,60,20,ID_LABEL17);

    return 0;
}

//Guarda datos del formulario en la clase data
int changeData(HWND hwnd,datos &data) {
    char buffer[500];
    GetDlgItemText(hwnd, ID_TEXT1, buffer, 501);
    data.nTime=atoi(buffer);
    GetDlgItemText(hwnd, ID_TEXT2, buffer, 501);
    data.nOut=atoi(buffer);

    GetDlgItemText(hwnd, ID_TEXT3, buffer, 501);
    data.nPartX=atoi(buffer);
    GetDlgItemText(hwnd, ID_TEXT4, buffer, 501);
    data.nPartY=atoi(buffer);

    GetDlgItemText(hwnd, ID_TEXT5, buffer, 501);
    data.mLon=atof(buffer);
    GetDlgItemText(hwnd, ID_TEXT6, buffer, 501);
    data.iLon=atof(buffer);
    GetDlgItemText(hwnd, ID_TEXT7, buffer, 501);
    data.mLat=atof(buffer);+
    GetDlgItemText(hwnd, ID_TEXT8, buffer, 501);
    data.iLat=atof(buffer);

    GetDlgItemText(hwnd, ID_TEXT9, buffer, 501);
    data.windFile.assign(buffer);
    GetDlgItemText(hwnd, ID_TEXT10, buffer, 501);
    data.tracFile.assign(buffer);

    GetDlgItemText(hwnd, ID_TEXT11, buffer, 501);
    data.isec=atoi(buffer);
    GetDlgItemText(hwnd, ID_TEXT12, buffer, 501);
    data.day=atoi(buffer);
    GetDlgItemText(hwnd, ID_TEXT13, buffer, 501);
    data.mon=atoi(buffer);
    GetDlgItemText(hwnd, ID_TEXT14, buffer, 501);
    data.year=atoi(buffer);

    GetDlgItemText(hwnd, ID_TEXT15, buffer, 501);
    data.theta=atof(buffer);

    GetDlgItemText(hwnd, ID_TEXT16, buffer, 501);
    data.its=atoi(buffer);

    GetDlgItemText(hwnd, ID_TEXT17, buffer, 501);
    data.gFile=atoi(buffer);
    return 0;
}

int NombreArchivo(HWND hwnd,string &fileName) {
    OPENFILENAME ofn;       // common dialog box structure
    char szFile[260];       // buffer for file name
    // owner window              // file handle

    // Initialize OPENFILENAME
    ZeroMemory(&ofn, sizeof(ofn));
    ofn.lStructSize = sizeof(ofn);
    ofn.hwndOwner = hwnd;
    ofn.lpstrFile = szFile;
    // Set lpstrFile[0] to '\0' so that GetOpenFileName does not
    // use the contents of szFile to initialize itself.
    ofn.lpstrFile[0] = '\0';
    ofn.nMaxFile = sizeof(szFile);
    ofn.lpstrFilter = "All\0*.*\0Text\0*.TXT\0";
    ofn.nFilterIndex = 1;
    ofn.lpstrFileTitle = NULL;
    ofn.nMaxFileTitle = 0;
    ofn.lpstrInitialDir = NULL;
    ofn.Flags = OFN_PATHMUSTEXIST;

    // Display the Open dialog box.

   // if (GetSaveFileName(&ofn)==TRUE) {
    //    fileName.assign(ofn.lpstrFile);
    //}

    return 0;
}

/*void * run_model(void *p) {
	trj_();
	//ExitProcess((int)p);
	pthread_exit(NULL);
	return 0;
}*/

/* Esta funci�n es llamada por la funci�n del API DispatchMessage() */
LRESULT CALLBACK WindowProcedure(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
    static datos data;
    static HBRUSH pincel;
	static int var=1;
	//static pthread_t thread;
	//static HANDLE Handle_Of_Thread_1 = 0;
	//int Data_Of_Thread_1 = 1;
    static string nombreArchivo;

    switch (msg)                /* manipulador del mensaje */
    {
        case WM_DESTROY:
			//pthread_cancel(thread);
			//pthread_exit(NULL);
			//CloseHandle(Handle_Of_Thread_1);
			//DeleteObject(pincel);
			//DeleteObject(hfont);
			PostQuitMessage(0);    /* env�a un mensaje WM_QUIT a la cola de mensajes */
			break;
        case WM_COMMAND:
            if (wParam == ID_BUTTON_GUARDAR) {
                changeData(hwnd,data);
                /*NombreArchivo(hwnd,nombreArchivo);
                if(!GuardarArchivo(nombreArchivo,data)) {
                    MessageBox(hwnd,(TCHAR*)"Archivo cfg guardado correctamente","",0);
                }*/
				if(MessageBox(hwnd,(TCHAR*)"Sobrescribir trj-2d.cfg","",MB_YESNO)==IDYES) {
					if(!GuardarArchivo("trj-2d.cfg",data)) {
	                    MessageBox(hwnd,(TCHAR*)"Archivo cfg guardado correctamente","",0);
	                }
				}
            } else if (wParam == ID_BUTTON_RESET ){
                //LeerArchivo("trj-2d.cfg",data);
                Resetear(hwnd,data);
            } else if (wParam == ID_BUTTON_CANCELAR) {
                SendMessage(hwnd,WM_CLOSE,wParam,lParam);
            //} else if (wParam == ID_BUTTON_RUN) {
				//if (var) {
					// variable to hold handle of Thread 1

					//run_model(&Data_Of_Thread_1);
					// Create thread 1.
					/*Handle_Of_Thread_1 = CreateThread( NULL, 0,
					run_model, &Data_Of_Thread_1, 0, NULL);
					if ( Handle_Of_Thread_1 == NULL)
						ExitProcess(Data_Of_Thread_1);*/

					/*pthread_create(&thread,NULL,run_model,NULL);
					var = 0;
				} else {
					MessageBox(hwnd,(TCHAR*)"Ya esta corriendo el modelo, no se hace nada","",0);
				}*/
			}
        break;
        case WM_CREATE:
           if(LeerArchivo("trj-2d.cfg",data)) {
						 MessageBox(hwnd,(TCHAR*)"Error al leer archivo trj-2d.cfg","",0);
					 }

           CrearControles(hwnd,lParam);
           Resetear(hwnd,data);

            CrearBoton(hwnd,lParam,36,300,60,20,ID_BUTTON_GUARDAR,"Guardar");
            CrearBoton(hwnd,lParam,106,300,60,20,ID_BUTTON_RESET,"Reset");
            CrearBoton(hwnd,lParam,176,300,60,20,ID_BUTTON_CANCELAR,"Cancelar");
           // CrearBoton(hwnd,lParam,246,300,60,20,ID_BUTTON_RUN,"RUN");
           //pincel = CreateSolidBrush(RGB(0,0,0));
				  // hfont = (HFONT)GetStockObject( DEFAULT_GUI_FONT );
           return 0;
        //case WM_CTLCOLOREDIT:
           //SetBkColor((HDC)wParam, RGB(0,0,0));
           //SetTextColor((HDC)wParam, RGB(255,255,255));
           //return (LRESULT)pincel;
					/* case WM_CTLCOLORSTATIC:
	      	 SetBkColor((HDC)wParam, GetSysColor(COLOR_BACKGROUND));
	         SetTextColor((HDC)wParam, RGB(255,255,255));
	        return (LRESULT)pincel;*/
        default:                  /* para los mensajes de los que no nos ocupamos */
           return DefWindowProc(hwnd, msg, wParam, lParam);
    }
    return 0;
}
